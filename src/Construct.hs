{-# LANGUAGE GADTs #-}

module Construct where

import qualified Control.Applicative as Applicative
import qualified Control.Monad.Fix as Monad.Fix
import Control.Applicative (Applicative, Alternative)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<$>), void)
import Data.Functor.Identity
import qualified Data.Functor.Const as Functor
import Data.Word (Word, Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Cancellative (LeftReductiveMonoid)
import Text.Grampa (InputParsing(ParserInput, anyToken, string))
import qualified Text.Parser.Combinators as Parser
import qualified Text.ParserCombinators.Incremental as Incremental
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser)
import Data.Serialize (Serialize, Result(Done, Fail, Partial), Get, Putter, runGetPartial, runPut)
import qualified Data.Serialize as Serialize

import qualified Rank2
import qualified Rank2.TH

-- | The central type. The four type parameters are:
--
--   * @m@ is the type of the parser for the format
--   * @n@ is the container type for the serialized form of the value, typically 'Identity' unless something
--     'Alternative' is called for.
--   * @s@ is the type of the serialized value, typically 'ByteString'
--   * @a@ is the type of the value in the program
--
-- The @parse@ and @serialize@ fields can be used to perform the two sides of the conversion between the in-memory and
-- serialized form of the value.
data Format m n s a = Format {
   parse :: m a,
   serialize :: a -> n s
   }

-- | A subclass of 'MonadFix' for monads that can fix a function that handles higher-kinded data
class MonadFix m => FixTraversable m where
   -- | This specialized form of 'Rank2.traverse' can be used inside 'mfix'.
   --
   -- > mfix (fixSequence . f) == fix (fixSequence . f =<<)
   fixSequence :: (Rank2.Traversable g, Applicative n) => g m -> m (g n)
   fixSequence = Rank2.traverse (pure <$>)

instance Monoid s => FixTraversable (Incremental.Parser t s) where
   fixSequence = Incremental.record

(<$)     :: (Eq a, Functor m, Alternative n) => a -> Format m n s () -> Format m n s a
(*>)     :: (Applicative m, Semigroup (n s)) => Format m n s () -> Format m n s a -> Format m n s a
(<*)     :: (Applicative m, Semigroup (n s)) => Format m n s a -> Format m n s () -> Format m n s a
(<|>)    :: (Alternative m, Alternative n) => Format m n s a -> Format m n s a -> Format m n s a
optional :: (Alternative m, Alternative n, Monoid (n s)) => Format m n s a -> Format m n s (Maybe a)
many     :: (Alternative m, Alternative n, Monoid (n s)) => Format m n s a -> Format m n s [a]
empty    :: (Alternative m, Alternative n) => Format m n s a
mfix     :: MonadFix m => (a -> Format m n s a) -> Format m n s a
literal  :: (Functor m, InputParsing m, Applicative n, ParserInput m ~ s) => s -> Format m n s ()
byte     :: (InputParsing m, ParserInput m ~ ByteString, Applicative n) => Format m n ByteString Word8
cereal   :: (Serialize a, Monad m, InputParsing m, ParserInput m ~ ByteString, Applicative n) => Format m n ByteString a
cereal'  :: (Monad m, InputParsing m, ParserInput m ~ ByteString, Applicative n) => Get a -> Putter a -> Format m n ByteString a
count    :: (Applicative m, Monoid (n s)) => Word -> Format m n s a -> Format m n s [a]
record   :: (Rank2.Apply g, Rank2.Traversable g, FixTraversable m, Monoid (n s), Applicative o, Foldable o) =>
            g (Format m n s) -> Format m n s (g o)

-- | A literal serialized form, such as a fixed prefix, corresponding to no value
literal s = Format{
   parse = void (string s),
   serialize = const (pure s)
   }

-- | A trivial format for a single byte
byte = Format{
   parse = ByteString.head <$> anyToken,
   serialize = pure . ByteString.singleton}

-- | A quick way to format a value that already has an appropriate 'Serialize' instance
cereal = cereal' Serialize.get Serialize.put

-- | Specifying a formatter explicitly using the cereal getter and putter
cereal' get put = Format p (pure . runPut . put)
   where p = go (runGetPartial get mempty)
            where go (Fail msg _) = fail msg
                  go (Done r _) = pure r
                  go (Partial cont) = anyToken >>= go . cont

-- | Repeats the argument format the given number of times.
count n item = Format{
   parse = Parser.count (fromIntegral n) (parse item),
   serialize = foldMap (serialize item)}

-- | Converts a record of field formats into single the format of the whole record.
record formats = Format{
   parse = fixSequence (parse Rank2.<$> formats),
   serialize = Rank2.foldMap Functor.getConst . Rank2.liftA2 serializeField formats
   }
   where serializeField format xs = Functor.Const (foldMap (serialize format) xs)

infixl 3 <|>
infixl 4 <$
infixl 4 <*
infixl 4 *>

-- | Same as the usual 'Data.Functor.<$' except a 'Format' is no 'Functor'.
a <$ f = Format{
   parse = a Applicative.<$ parse f,
   serialize = \b-> if a == b then serialize f () else Applicative.empty}

-- | Same as the usual 'Applicative.*>' except a 'Format' is no 'Functor', let alone 'Applicative'.
f1 *> f2 = Format{
   parse = parse f1 Applicative.*> parse f2,
   serialize = \a-> serialize f1 () <> serialize f2 a}

-- | Same as the usual 'Applicative.<*' except a 'Format' is no 'Functor', let alone 'Applicative'.
f1 <* f2 = Format{
   parse = parse f1 Applicative.<* parse f2,
   serialize = \a-> serialize f1 a <> serialize f2 ()}

-- | Same as the usual 'Applicative.<|>' except a 'Format' is no 'Functor', let alone 'Alternative'.
f1 <|> f2 = Format{
   parse = parse f1 Applicative.<|> parse f2,
   serialize = \a-> serialize f1 a Applicative.<|> serialize f2 a}

-- | Same as the usual 'Applicative.optional' except a 'Format' is no 'Functor', let alone 'Alternative'.
optional f = Format{
   parse = Applicative.optional (parse f),
   serialize = maybe mempty (serialize f)}

-- | Same as the usual 'Applicative.many' except a 'Format' is no 'Functor', let alone 'Alternative'.
many f = Format{
   parse = Applicative.many (parse f),
   serialize = foldMap (serialize f)}

-- | Same as the usual 'Applicative.empty' except a 'Format' is no 'Functor', let alone 'Alternative'.
empty = Format{
   parse = Applicative.empty,
   serialize = const Applicative.empty}

-- | Same as the usual 'Control.Monad.Fix.mfix' except a 'Format' is no 'Functor', let alone 'Monad'.
mfix f = Format{
   parse = Monad.Fix.mfix (parse . f),
   serialize = \a-> serialize (f a) a}

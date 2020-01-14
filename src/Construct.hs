{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}

module Construct
(
  -- * The type
  Format, parse, serialize,

  -- * Combinators
  (Construct.<$), (Construct.*>), (Construct.<*), (Construct.<|>),
  empty, optional, optionWithDefault, many, count,
  -- ** Self-referential record format support
  mfix, record,
  -- ** Mapping over a 'Format'
  mapSerialized, mapMaybeSerialized, mapValue, mapMaybeValue,
  -- ** Constraining a 'Format'
  value, padded, padded1,

  -- * Primitives
  literal, byte, char,
  cereal, cereal',
  Construct.take, Construct.takeWhile, takeWhile1, takeCharsWhile, takeCharsWhile1
) where

import qualified Control.Applicative as Applicative
import qualified Control.Monad.Fix as Monad.Fix
import Control.Applicative (Applicative, Alternative)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<$>), void)
import Data.Functor.Identity
import qualified Data.Functor.Const as Functor
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Word (Word, Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import qualified Data.Monoid.Null as Null
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Textual (TextualMonoid)
import Data.Semigroup.Cancellative (LeftReductive)
import Data.String (IsString, fromString)
import Text.Grampa (InputParsing(ParserInput, anyToken, string), InputCharParsing)
import qualified Text.Grampa as Grampa
import qualified Text.Parser.Combinators as Parser
import qualified Text.Parser.Char as Parser.Char
import qualified Text.ParserCombinators.Incremental as Incremental
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser, LeftBiasedLocal)
import Data.Serialize (Serialize, Result(Done, Fail, Partial), Get, Putter, runGetPartial, runPut)
import qualified Data.Serialize as Serialize

import qualified Rank2

import Construct.Classes
import Construct.Internal

(<$)     :: (Eq a, Functor m, Alternative n) => a -> Format m n s () -> Format m n s a
(*>)     :: (Applicative m, Semigroup (n s)) => Format m n s () -> Format m n s a -> Format m n s a
(<*)     :: (Applicative m, Semigroup (n s)) => Format m n s a -> Format m n s () -> Format m n s a
(<|>)    :: (Alternative m, Alternative n) => Format m n s a -> Format m n s a -> Format m n s a
optional :: (Alternative m, Alternative n, Monoid (n s)) => Format m n s a -> Format m n s (Maybe a)
many     :: (Alternative m, Alternative n, Monoid (n s)) => Format m n s a -> Format m n s [a]
empty    :: (Alternative m, Alternative n) => Format m n s a
mfix     :: MonadFix m => (a -> Format m n s a) -> Format m n s a
literal  :: (Functor m, InputParsing m, Applicative n, ParserInput m ~ s) => s -> Format m n s ()
value    :: (Eq a, Alternative m, Monad m, Alternative n) => Format m n s a -> a -> Format m n s ()
byte     :: (InputParsing m, ParserInput m ~ ByteString, Applicative n) => Format m n ByteString Word8
char     :: (Parser.Char.CharParsing m, IsString (ParserInput m), Applicative n) => Format m n ByteString Char
cereal   :: (Serialize a, Monad m, InputParsing m, ParserInput m ~ ByteString, Applicative n) => Format m n ByteString a
cereal'  :: (Monad m, InputParsing m, ParserInput m ~ ByteString, Applicative n) =>
            Get a -> Putter a -> Format m n ByteString a
count    :: (Applicative m, Monoid (n s)) => Int -> Format m n s a -> Format m n s [a]
record   :: (Rank2.Apply g, Rank2.Traversable g, FixTraversable m, Monoid (n s), Applicative o, Foldable o) =>
            g (Format m n s) -> Format m n s (g o)

-- | A literal serialized form, such as a fixed prefix, corresponding to no value
literal s = Format{
   parse = void (string s),
   serialize = const (pure s)
   }

-- | Modifies the serialized form of the given format by padding it with the given template if it's any shorter
padded :: (Monad m, Functor n, InputParsing m, ParserInput m ~ s, FactorialMonoid s) =>
          s -> Format m n s s -> Format m n s s
padded template format = Format{
   parse = parse format >>= parsePadding,
   serialize = (padRight <$>) . serialize format
   }
   where padRight s = s <> Factorial.drop (Factorial.length s) template
         parsePadding s = if Null.null padding then pure s else s Applicative.<$ Grampa.string padding
            where padding = Factorial.drop (Factorial.length s) template

-- | Modifies the serialized form of the given format by padding it with the given template. The serialized form has
-- to be shorter than the template before padding.
padded1 :: (Monad m, Monad n, InputParsing m, ParserInput m ~ s, FactorialMonoid s, Alternative n) =>
           s -> Format m n s s -> Format m n s s
padded1 template format = Format{
   parse = parse format >>= parsePadding,
   serialize = \a-> serialize format a >>= padRight
   }
   where padRight s = if Null.null padding then Applicative.empty else pure (s <> padding)
            where padding = Factorial.drop (Factorial.length s) template
         parsePadding s = if Null.null padding then Applicative.empty else s Applicative.<$ Grampa.string padding
            where padding = Factorial.drop (Factorial.length s) template

-- | Format whose in-memory value is a fixed-size prefix of the serialized value
take :: (InputParsing m, ParserInput m ~ s, FactorialMonoid s, Alternative n) => Int -> Format m n s s
take n = Format{
   parse = mconcat <$> Parser.count n anyToken,
   serialize = \s-> if Factorial.length s == n then pure s else Applicative.empty
   }

-- | Format whose in-memory value is the longest prefix of the serialized value whose smallest parts all satisfy the
-- given predicate.
takeWhile :: (InputParsing m, ParserInput m ~ s, FactorialMonoid s, Alternative n) => (s -> Bool) -> Format m n s s
takeWhile pred = Format{
   parse = Grampa.takeWhile pred,
   serialize = \s-> if Null.null s || Null.null (Factorial.dropWhile pred s) then pure s else Applicative.empty
   }

-- | Format whose in-memory value is the longest non-empty prefix of the serialized value whose smallest parts all
-- satisfy the given predicate.
takeWhile1 :: (InputParsing m, ParserInput m ~ s, FactorialMonoid s, Alternative n) => (s -> Bool) -> Format m n s s
takeWhile1 pred = Format{
   parse = Grampa.takeWhile1 pred,
   serialize = \s-> if not (Null.null s) && Null.null (Factorial.dropWhile pred s) then pure s else Applicative.empty
   }

-- | Format whose in-memory value is the longest prefix of the serialized value that consists of characters which all
-- satisfy the given predicate.
takeCharsWhile :: (InputCharParsing m, ParserInput m ~ s, TextualMonoid s, Alternative n) =>
                  (Char -> Bool) -> Format m n s s
takeCharsWhile pred = Format{
   parse = Grampa.takeCharsWhile pred,
   serialize = \s-> if Null.null s || Null.null (Textual.dropWhile_ False pred s) then pure s else Applicative.empty
   }

-- | Format whose in-memory value is the longest non-empty prefix of the serialized value that consists of characters
-- which all satisfy the given predicate.
takeCharsWhile1 :: (InputCharParsing m, ParserInput m ~ s, TextualMonoid s, Alternative n) =>
                   (Char -> Bool) -> Format m n s s
takeCharsWhile1 pred = Format{
   parse = Grampa.takeCharsWhile1 pred,
   serialize = \s-> if not (Null.null s) && Null.null (Textual.dropWhile_ False pred s) then pure s
                    else Applicative.empty
   }

-- | A fixed expected value serialized through the agument format
value f v = Format{
   parse = void (parse f >>= \x-> if x == v then pure x else Applicative.empty),
   serialize = \()-> serialize f v
   }

-- | Converts a format for serialized streams of type @s@ so it works for streams of type @t@ instead
mapSerialized :: (Monoid s, Monoid t, InputParsing (m s), InputParsing (m t),
                  s ~ ParserInput (m s), t ~ ParserInput (m t), InputMappableParsing m, Functor n) =>
                 (s -> t) -> (t -> s) -> Format (m s) n s a -> Format (m t) n t a
mapSerialized f f' format = Format{
   parse = mapParserInput f f' (parse format),
   serialize = (f <$>) . serialize format}

-- | Converts a format for serialized streams of type @s@ so it works for streams of type @t@ instead.
mapMaybeSerialized :: (Monoid s, Monoid t, InputParsing (m s), InputParsing (m t),
                       s ~ ParserInput (m s), t ~ ParserInput (m t), InputMappableParsing m, Functor n) =>
                      (s -> Maybe t) -> (t -> Maybe s) -> Format (m s) n s a -> Format (m t) n t a
mapMaybeSerialized f f' format = Format{
   parse = mapMaybeParserInput f f' (parse format),
   serialize = (fromMaybe (error "Partial serialization") . f <$>) . serialize format}

-- | Converts a format for in-memory values of type @a@ so it works for values of type @b@ instead
mapValue :: Functor m => (a -> b) -> (b -> a) -> Format m n s a -> Format m n s b
mapValue f f' format = Format{
   parse = f <$> parse format,
   serialize = serialize format . f'}

-- | Converts a format for in-memory values of type @a@ so it works for values of type @b@ instead
mapMaybeValue :: (Monad m, Alternative m, Alternative n) =>
                 (a -> Maybe b) -> (b -> Maybe a) -> Format m n s a -> Format m n s b
mapMaybeValue f f' format = Format{
   parse = parse format >>= maybe Applicative.empty pure . f,
   serialize = maybe Applicative.empty (serialize format) . f'}

-- | A trivial format for a single byte in a 'ByteString'
byte = Format{
   parse = ByteString.head <$> anyToken,
   serialize = pure . ByteString.singleton}

-- | A trivial format for a single character
char = Format{
   parse = Parser.Char.anyChar,
   serialize = pure . fromString . (:[])}

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

-- | Converts a record of field formats into a single format of the whole record.
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

-- | Like 'optional' except with arbitrary default serialization for the @Nothing@ value.
-- > optional = optionWithDefault (literal mempty)
optionWithDefault :: (Alternative m, Alternative n, Monoid (n s)) =>
                     Format m n s () -> Format m n s a -> Format m n s (Maybe a)
optionWithDefault d f = Format{
   parse = Just <$> parse f Applicative.<|> Nothing Applicative.<$ parse d,
   serialize = maybe (serialize d ()) (serialize f)}

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

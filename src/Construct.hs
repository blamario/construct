{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TemplateHaskell, TypeApplications, TypeFamilies #-}

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
import Text.Grampa (InputParsing(ParserInput, anyToken, getInput, string))
import qualified Text.Parser.Combinators as Parser
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Text.ParserCombinators.Incremental as Incremental
import Text.ParserCombinators.Incremental.Symmetric (Symmetric)
import Data.Serialize (Serialize, Result(Done, Fail, Partial), Get, Putter, runGetPartial, runPut)
import qualified Data.Serialize as Serialize

import qualified Rank2
import qualified Rank2.TH

import Prelude hiding ((*>), (<*))

data BitMap f = BitMap{
   width :: f Word8,
   height :: f Word8,
   pixels :: f [[Word8]]
   }

deriving instance Show (BitMap Identity)

$(Rank2.TH.deriveAll ''BitMap)

format :: Format (Incremental.Parser Symmetric ByteString) Identity ByteString (BitMap Identity)
format = literal (ASCII.pack "BMP") *> mfix (\r-> record
  BitMap{
        width= cereal,
        height= cereal,
        pixels= count (fromIntegral $ height r) (count (fromIntegral $ width r) cereal)
        })

data Format m n s a = Format {
   parse :: m a,
   serialize :: a -> n s
   }

(<$)    :: (Eq a, Functor m, Alternative n) => a -> Format m n s () -> Format m n s a
(*>)    :: (Applicative m, Semigroup (n s)) => Format m n s () -> Format m n s a -> Format m n s a
(<*)    :: (Applicative m, Semigroup (n s)) => Format m n s a -> Format m n s () -> Format m n s a
(<|>)   :: (Alternative m, Alternative n) => Format m n s a -> Format m n s a -> Format m n s a
empty   :: (Alternative m, Alternative n) => Format m n s a
mfix    :: MonadFix m => (a -> Format m n s a) -> Format m n s a
literal :: (Functor m, InputParsing m, Applicative n, ParserInput m ~ s) => s -> Format m n s ()
byte    :: (InputParsing m, ParserInput m ~ ByteString) => Format m Identity ByteString Word8
cereal  :: (Serialize a, Monad m, InputParsing m, ParserInput m ~ ByteString) => Format m Identity ByteString a
cereal' :: (Monad m, InputParsing m, ParserInput m ~ ByteString) => Get a -> Putter a -> Format m Identity ByteString a
count   :: (Applicative m, Monoid (n s)) => Word -> Format m n s a -> Format m n s [a]
--record  :: (Rank2.Apply g, Rank2.Traversable g, Applicative m, Monoid (n s)) => g (Format m n s) -> Format m n s (g Identity)
record  :: (Rank2.Apply g, Rank2.Traversable g, Monoid s, Monoid (n s)) =>
           g (Format (Incremental.Parser Symmetric s) n s) -> Format (Incremental.Parser Symmetric s) n s (g Identity)

literal s = Format{
   parse = void (string s),
   serialize = const (pure s)
   }

byte = Format{
   parse = ByteString.head <$> anyToken,
   serialize = Identity . ByteString.singleton}

cereal = cereal' Serialize.get Serialize.put

cereal' get put = Format p (Identity . runPut . put)
   where p = go (runGetPartial get mempty)
            where go (Fail msg _) = fail msg
                  go (Done r _) = pure r
                  go (Partial cont) = anyToken >>= go . cont

count n item = Format{
   parse = Parser.count (fromIntegral n) (parse item),
   serialize = foldMap (serialize item)}

record formats = Format{
--   parse = Rank2.traverse (fmap Identity . parse) formats,
   parse = Incremental.record (parse Rank2.<$> formats),
   serialize = Rank2.foldMap Functor.getConst . Rank2.liftA2 serializeField formats
   }
   where serializeField format (Identity a) = Functor.Const (serialize format a)

a <$ f = Format{
   parse = a Applicative.<$ parse f,
   serialize = \b-> if a == b then serialize f () else Applicative.empty}

f1 *> f2 = Format{
   parse = parse f1 Applicative.*> parse f2,
   serialize = \a-> serialize f1 () <> serialize f2 a}

f1 <* f2 = Format{
   parse = parse f1 Applicative.<* parse f2,
   serialize = \a-> serialize f1 a <> serialize f2 ()}

f1 <|> f2 = Format{
   parse = parse f1 Applicative.<|> parse f2,
   serialize = \a-> serialize f1 a Applicative.<|> serialize f2 a}

empty = Format{
   parse = Applicative.empty,
   serialize = const Applicative.empty}

mfix f = Format{
   parse = Monad.Fix.mfix (parse . f),
   serialize = \a-> serialize (f a) a}

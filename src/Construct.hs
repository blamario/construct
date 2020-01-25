{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables #-}

module Construct
(
  -- * The type
  Format, parse, serialize,

  -- * Combinators
  (Construct.<$), (Construct.*>), (Construct.<*), (Construct.<|>), (<+>), (<?>),
  empty, optional, optionWithDefault, pair, deppair, many, some, sepBy, count,
  -- ** Self-referential record support
  mfix, record, recordWith,
  -- ** Mapping over a 'Format'
  mapSerialized, mapMaybeSerialized, mapValue, mapMaybeValue,
  -- ** Constraining a 'Format'
  satisfy, value, padded, padded1,

  -- * Primitives
  literal, byte, char,
  cereal, cereal',
  Construct.take, Construct.takeWhile, Construct.takeWhile1, Construct.takeCharsWhile, Construct.takeCharsWhile1,
  -- * Test helpers
  testParse, testSerialize
) where

import qualified Control.Applicative as Applicative
import qualified Control.Monad.Fix as Monad.Fix
import Control.Applicative (Applicative, Alternative)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<$>), void)
import qualified Data.Functor.Const as Functor
import qualified Data.Functor.Identity as Functor
import qualified Data.List as List
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(Ap, getAp))
import Data.Semigroup (Semigroup, (<>), sconcat)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import qualified Data.Monoid.Null as Null
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Textual (TextualMonoid)
import Data.String (IsString, fromString)
import qualified Text.Parser.Combinators as Parser
import qualified Text.Parser.Char as Parser.Char
import qualified Text.ParserCombinators.Incremental as Incremental
import Text.ParserCombinators.Incremental.Symmetric (Symmetric)
import Data.Serialize (Serialize, Result(Done, Fail, Partial), Get, Putter, runGetPartial, runPut)
import qualified Data.Serialize as Serialize

import qualified Rank2

import qualified Construct.Classes as Input
import Construct.Classes (AlternativeFail(failure), InputParsing (ParserInput), InputCharParsing, InputMappableParsing,
                          FixTraversable, Error, errorString, expectedName)
import Construct.Internal

import Prelude hiding (pred, take, takeWhile)

-- $setup
-- >>> import Data.Char (isDigit, isLetter)
-- >>> import Data.Serialize.Get (getWord16le)
-- >>> import Data.Serialize.Put (putWord16le)
-- >>> import Data.Word (Word16)
-- >>> import Numeric (showInt)

literal  :: (Functor m, InputParsing m, Applicative n, ParserInput m ~ s) => s -> Format m n s ()
-- | A literal serialized form, such as a magic constant, corresponding to no value
--
-- >>> testParse (literal "Hi") "Hi there"
-- Right [(()," there")]
literal s = Format{
   parse = void (Input.string s),
   serialize = const (pure s)
   }

-- | Modifies the serialized form of the given format by padding it with the given template if it's any shorter
--
-- >>> testParse (padded "----" $ takeCharsWhile isDigit) "12--3---"
-- Right [("12","3---")]
-- >>> testSerialize (padded "----" $ takeCharsWhile isDigit) "12"
-- Right "12--"
padded :: (Monad m, Functor n, InputParsing m, ParserInput m ~ s, FactorialMonoid s) =>
          s -> Format m n s s -> Format m n s s
padded template format = Format{
   parse = parse format >>= parsePadding,
   serialize = (padRight <$>) . serialize format
   }
   where padRight s = s <> Factorial.drop (Factorial.length s) template
         parsePadding s = if Null.null padding then pure s else s Applicative.<$ Input.string padding
            where padding = Factorial.drop (Factorial.length s) template

-- | Modifies the serialized form of the given format by padding it with the given template. The serialized form has
-- to be shorter than the template before padding.
padded1 :: (Monad m, Monad n, InputParsing m, ParserInput m ~ s, FactorialMonoid s, Show s, AlternativeFail n) =>
           s -> Format m n s s -> Format m n s s
padded1 template format = Format{
   parse = parse format >>= parsePadding,
   serialize = \a-> serialize format a >>= padRight
   }
   where padRight s = if Null.null padding then expectedName ("padded1 " ++ show template) (failure $ show s)
                      else pure (s <> padding)
            where padding = Factorial.drop (Factorial.length s) template
         parsePadding s = if Null.null padding
                          then Parser.unexpected (show s) Parser.<?> ("padded1 " ++ show template)
                          else s Applicative.<$ Input.string padding
            where padding = Factorial.drop (Factorial.length s) template

-- | Format whose in-memory value is a fixed-size prefix of the serialized value
--
-- >>> testParse (take 3) "12345"
-- Right [("123","45")]
-- >>> testSerialize (take 3) "123"
-- Right "123"
-- >>> testSerialize (take 3) "1234"
-- Left "expected a value of length 3, encountered \"1234\""
take :: (InputParsing m, ParserInput m ~ s, FactorialMonoid s, Show s, AlternativeFail n) => Int -> Format m n s s
take n = Format{
   parse = Input.take n,
   serialize = \s-> if Factorial.length s == n then pure s
                    else expectedName ("a value of length " ++ show n) (failure $ show s)
   }

-- | Format whose in-memory value is the longest prefix of the serialized value smallest parts of which all satisfy
-- the given predicate.
--
-- >>> testParse (takeWhile (> "b")) "abcd"
-- Right [("","abcd")]
-- >>> testParse (takeWhile (> "b")) "dcba"
-- Right [("dc","ba")]
-- >>> testSerialize (takeWhile (> "b")) "dcba"
-- Left "expected takeWhile, encountered \"dcba\""
-- >>> testSerialize (takeWhile (> "b")) "dc"
-- Right "dc"
-- >>> testSerialize (takeWhile (> "b")) ""
-- Right ""
takeWhile :: (InputParsing m, ParserInput m ~ s, FactorialMonoid s, Show s, AlternativeFail n) =>
             (s -> Bool) -> Format m n s s
takeWhile pred = Format{
   parse = Input.takeWhile pred,
   serialize = \s-> if Null.null (Factorial.dropWhile pred s) then pure s
                    else expectedName "takeWhile" (failure $ show s)
   }

-- | Format whose in-memory value is the longest non-empty prefix of the serialized value smallest parts of which all
-- satisfy the given predicate.
--
-- >>> testParse (takeWhile1 (> "b")) "abcd"
-- Left "takeWhile1"
-- >>> testSerialize (takeWhile1 (> "b")) ""
-- Left "expected takeWhile1, encountered \"\""
-- >>> testSerialize (takeWhile1 (> "b")) "dc"
-- Right "dc"
takeWhile1 :: (InputParsing m, ParserInput m ~ s, FactorialMonoid s, Show s, AlternativeFail n) =>
              (s -> Bool) -> Format m n s s
takeWhile1 pred = Format{
   parse = Input.takeWhile1 pred,
   serialize = \s-> if not (Null.null s) && Null.null (Factorial.dropWhile pred s) then pure s
                    else expectedName "takeWhile1" (failure $ show s)
   }

-- | Format whose in-memory value is the longest prefix of the serialized value that consists of characters which all
-- satisfy the given predicate.
--
-- >>> testParse (takeCharsWhile isDigit) "a12"
-- Right [("","a12")]
-- >>> testParse (takeCharsWhile isDigit) "12a"
-- Right [("12","a")]
-- >>> testSerialize (takeCharsWhile isDigit) "12a"
-- Left "expected takeCharsWhile, encountered \"12a\""
-- >>> testSerialize (takeCharsWhile isDigit) "12"
-- Right "12"
-- >>> testSerialize (takeCharsWhile isDigit) ""
-- Right ""
takeCharsWhile :: (InputCharParsing m, ParserInput m ~ s, TextualMonoid s, Show s, AlternativeFail n) =>
                  (Char -> Bool) -> Format m n s s
takeCharsWhile pred = Format{
   parse = Input.takeCharsWhile pred,
   serialize = \s-> if Null.null (Textual.dropWhile_ False pred s) then pure s
                    else expectedName "takeCharsWhile" (failure $ show s)
   }

-- | Format whose in-memory value is the longest non-empty prefix of the serialized value that consists of characters
-- which all satisfy the given predicate.
--
-- >>> testParse (takeCharsWhile1 isDigit) "a12"
-- Left "takeCharsWhile1 encountered 'a'"
-- >>> testParse (takeCharsWhile1 isDigit) "12a"
-- Right [("12","a")]
-- >>> testSerialize (takeCharsWhile1 isDigit) "12"
-- Right "12"
-- >>> testSerialize (takeCharsWhile1 isDigit) ""
-- Left "expected takeCharsWhile1, encountered \"\""
takeCharsWhile1 :: (InputCharParsing m, ParserInput m ~ s, TextualMonoid s, Show s, AlternativeFail n) =>
                   (Char -> Bool) -> Format m n s s
takeCharsWhile1 pred = Format{
   parse = Input.takeCharsWhile1 pred,
   serialize = \s-> if not (Null.null s) && Null.null (Textual.dropWhile_ False pred s) then pure s
                    else expectedName "takeCharsWhile1" (failure $ show s)
   }

value :: (Eq a, Show a, Parser.Parsing m, Monad m, Alternative n) => Format m n s a -> a -> Format m n s ()
-- | A fixed expected value serialized through the argument format
--
-- >>> testParse (value char 'a') "bcd"
-- Left "encountered 'b'"
-- >>> testParse (value char 'a') "abc"
-- Right [((),"bc")]
value f v = Format{
   parse = void (parse f >>= \x-> if x == v then pure x else Parser.unexpected (show x)),
   serialize = \()-> serialize f v
   }

satisfy :: (Parser.Parsing m, Monad m, AlternativeFail n, Show a) => (a -> Bool) -> Format m n s a -> Format m n s a
-- | Filter the argument format so it only succeeds for values that pass the predicate.
--
-- >>> testParse (satisfy isDigit char) "abc"
-- Left "encountered 'a'"
-- >>> testParse (satisfy isLetter char) "abc"
-- Right [('a',"bc")]
satisfy predicate f = Format{
   parse = parse f >>= \v-> if predicate v then pure v else Parser.unexpected (show v),
   serialize = \v-> if predicate v then serialize f v else expectedName "satisfy" (failure $ show v)
   }

-- | Converts a format for serialized streams of type @s@ so it works for streams of type @t@ instead
--
-- >>> testParse (mapSerialized ByteString.unpack ByteString.pack byte) [1,2,3]
-- Right [(1,[2,3])]
mapSerialized :: (Monoid s, Monoid t, InputParsing (m s), InputParsing (m t),
                  s ~ ParserInput (m s), t ~ ParserInput (m t), InputMappableParsing m, Functor n) =>
                 (s -> t) -> (t -> s) -> Format (m s) n s a -> Format (m t) n t a
mapSerialized f f' format = Format{
   parse = Input.mapParserInput f f' (parse format),
   serialize = (f <$>) . serialize format}

-- | Converts a format for serialized streams of type @s@ so it works for streams of type @t@ instead. The argument
-- functions may return @Nothing@ to indicate they have insuficient input to perform the conversion.
mapMaybeSerialized :: (Monoid s, Monoid t, InputParsing (m s), InputParsing (m t),
                       s ~ ParserInput (m s), t ~ ParserInput (m t), InputMappableParsing m, Functor n) =>
                      (s -> Maybe t) -> (t -> Maybe s) -> Format (m s) n s a -> Format (m t) n t a
mapMaybeSerialized f f' format = Format{
   parse = Input.mapMaybeParserInput f f' (parse format),
   serialize = (fromMaybe (error "Partial serialization") . f <$>) . serialize format}

-- | Converts a format for in-memory values of type @a@ so it works for values of type @b@ instead.
--
-- >>> testParse (mapValue (read @Int) show $ takeCharsWhile1 isDigit) "012 34"
-- Right [(12," 34")]
-- >>> testSerialize (mapValue read show $ takeCharsWhile1 isDigit) 12
-- Right "12"
mapValue :: Functor m => (a -> b) -> (b -> a) -> Format m n s a -> Format m n s b
mapValue f f' format = Format{
   parse = f <$> parse format,
   serialize = serialize format . f'}

-- | Converts a format for in-memory values of type @a@ so it works for values of type @b@ instead. The argument
-- functions may signal conversion failure by returning @Nothing@.
mapMaybeValue :: (Monad m, Parser.Parsing m, Show a, Show b, AlternativeFail n) =>
                 (a -> Maybe b) -> (b -> Maybe a) -> Format m n s a -> Format m n s b
mapMaybeValue f f' format = Format{
   parse = parse format >>= \v-> maybe (Parser.unexpected $ show v) pure (f v),
   serialize = \v-> maybe (expectedName "mapMaybeValue" (failure $ show v)) (serialize format) (f' v)}

byte :: (InputParsing m, ParserInput m ~ ByteString, Applicative n) => Format m n ByteString Word8
-- | A trivial format for a single byte in a 'ByteString'
--
-- >>> testParse byte (ByteString.pack [1,2,3])
-- Right [(1,"\STX\ETX")]
byte = Format{
   parse = ByteString.head <$> Input.anyToken,
   serialize = pure . ByteString.singleton}

char     :: (Parser.Char.CharParsing m, ParserInput m ~ s, IsString s, Applicative n) => Format m n s Char
-- | A trivial format for a single character
--
-- >>> testParse char "abc"
-- Right [('a',"bc")]
char = Format{
   parse = Parser.Char.anyChar,
   serialize = pure . fromString . (:[])}

cereal :: (Serialize a, Monad m, InputParsing m, ParserInput m ~ ByteString, Applicative n) => Format m n ByteString a
-- | A quick way to format a value that already has an appropriate 'Serialize' instance
--
-- >>> testParse (cereal @Word16) (ByteString.pack [1,2,3])
-- Right [(258,"\ETX")]
-- >>> testSerialize cereal (1025 :: Word16)
-- Right "\EOT\SOH"
cereal = cereal' Serialize.get Serialize.put

cereal' :: (Monad m, InputParsing m, ParserInput m ~ ByteString, Applicative n) =>
            Get a -> Putter a -> Format m n ByteString a
-- | Specifying a formatter explicitly using the cereal getter and putter
--
-- >>> testParse (cereal' getWord16le putWord16le) (ByteString.pack [1,2,3])
-- Right [(513,"\ETX")]
cereal' get put = Format p (pure . runPut . put)
   where p = go (runGetPartial get mempty)
            where go (Fail msg _) = Parser.unexpected msg
                  go (Done r _) = pure r
                  go (Partial cont) = Input.anyToken >>= go . cont

count :: (Applicative m, AlternativeFail n, Show a, Monoid s) => Int -> Format m n s a -> Format m n s [a]
-- | Repeats the argument format the given number of times.
--
-- >>> testParse (count 4 byte) (ByteString.pack [1,2,3,4,5])
-- Right [([1,2,3,4],"\ENQ")]
-- >>> testSerialize (count 4 byte) [1,2,3,4,5]
-- Left "expected a list of length 4, encountered [1,2,3,4,5]"
-- >>> testSerialize (count 4 byte) [1,2,3,4]
-- Right "\SOH\STX\ETX\EOT"
count n item = Format{
   parse = Parser.count (fromIntegral n) (parse item),
   serialize = \as-> if length as == n then mconcat <$> traverse (serialize item) as
                     else expectedName ("a list of length " ++ show n) (failure $ show as)}

record :: (Rank2.Apply g, Rank2.Traversable g, FixTraversable m, Applicative n, Monoid s) =>
          g (Format m n s) -> Format m n s (g Functor.Identity)
-- | Converts a record of field formats into a single format of the whole record.
record = recordWith Functor.runIdentity

recordWith :: forall g m n o s. (Rank2.Apply g, Rank2.Traversable g, FixTraversable m, Applicative n, Monoid s,
                                 Applicative o) =>
              (forall a. o (n a) -> n a) -> g (Format m n s) -> Format m n s (g o)
-- | Converts a record of field formats into a single format of the whole record, a generalized form of 'record'.
recordWith collapse formats = Format{
   parse = Input.fixSequence (parse Rank2.<$> formats),
   serialize = getAp . Rank2.foldMap Functor.getConst . Rank2.liftA2 serializeField formats
   }
   where serializeField :: forall a. Format m n s a -> o a -> Functor.Const (Ap n s) a
         serializeField format xs = Functor.Const (Ap $ collapse (serialize format <$> xs))

infixl 3 <|>
infixl 4 <$
infixl 4 <*
infixl 4 *>

(<$) :: (Eq a, Show a, Functor m, AlternativeFail n) => a -> Format m n s () -> Format m n s a
-- | Same as the usual 'Data.Functor.<$' except a 'Format' is no 'Functor'.
a <$ f = Format{
   parse = a Applicative.<$ parse f,
   serialize = \b-> if a == b then serialize f () else expectedName (show a) (failure $ show b)}

(*>) :: (Applicative m, Applicative n, Semigroup s) => Format m n s () -> Format m n s a -> Format m n s a
-- | Same as the usual 'Applicative.*>' except a 'Format' is no 'Functor', let alone 'Applicative'.
f1 *> f2 = Format{
   parse = parse f1 Applicative.*> parse f2,
   serialize = \a-> Applicative.liftA2 (<>) (serialize f1 ()) (serialize f2 a)}

(<*) :: (Applicative m, Applicative n, Semigroup s) => Format m n s a -> Format m n s () -> Format m n s a
-- | Same as the usual 'Applicative.<*' except a 'Format' is no 'Functor', let alone 'Applicative'.
f1 <* f2 = Format{
   parse = parse f1 Applicative.<* parse f2,
   serialize = \a-> Applicative.liftA2 (<>) (serialize f1 a) (serialize f2 ())}

(<|>) :: (Alternative m, Alternative n) => Format m n s a -> Format m n s a -> Format m n s a
-- | Same as the usual 'Applicative.<|>' except a 'Format' is no 'Functor', let alone 'Alternative'.
f1 <|> f2 = Format{
   parse = parse f1 Applicative.<|> parse f2,
   serialize = \a-> serialize f1 a Applicative.<|> serialize f2 a}

(<+>) :: Alternative m => Format m n s a -> Format m n s b -> Format m n s (Either a b)
-- | A discriminated or tagged choice between two formats.
f1 <+> f2 = Format{
   parse = Left <$> parse f1 Applicative.<|> Right <$> parse f2,
   serialize = either (serialize f1) (serialize f2)}

optional :: (Alternative m, Alternative n, Monoid s) => Format m n s a -> Format m n s (Maybe a)
-- | Same as the usual 'Applicative.optional' except a 'Format' is no 'Functor', let alone 'Alternative'.
optional f = Format{
   parse = Applicative.optional (parse f),
   serialize = maybe (pure mempty) (serialize f)}

-- | Like 'optional' except with arbitrary default serialization for the @Nothing@ value.
--
-- > optional = optionWithDefault (literal mempty)
optionWithDefault :: (Alternative m, Alternative n) => Format m n s () -> Format m n s a -> Format m n s (Maybe a)
optionWithDefault d f = Format{
   parse = Just <$> parse f Applicative.<|> Nothing Applicative.<$ parse d,
   serialize = maybe (serialize d ()) (serialize f)}

many :: (Alternative m, Applicative n, Monoid s) => Format m n s a -> Format m n s [a]
-- | Same as the usual 'Applicative.many' except a 'Format' is no 'Functor', let alone 'Alternative'.
many f = Format{
   parse = Applicative.many (parse f),
   serialize = fmap mconcat . traverse (serialize f)}

some :: (Alternative m, AlternativeFail n, Semigroup s) => Format m n s a -> Format m n s [a]
-- | Same as the usual 'Applicative.some' except a 'Format' is no 'Functor', let alone 'Alternative'.
some f = Format{
   parse = Applicative.some (parse f),
   serialize = maybe (failure "[]") (fmap sconcat . traverse (serialize f)) . nonEmpty}

sepBy :: (Alternative m, Applicative n, Monoid s) => Format m n s a -> Format m n s () -> Format m n s [a]
-- | Represents any number of values formatted using the first argument, separated by the second format argumewnt in
-- serialized form. Similar to the usual 'Parser.sepBy' combinator.
--
-- >>> testParse (takeCharsWhile isLetter `sepBy` literal ",") "foo,bar,baz"
-- Right [([],"foo,bar,baz"),(["foo"],",bar,baz"),(["foo","bar"],",baz"),(["foo","bar","baz"],"")]
sepBy format separator = Format{
   parse = Parser.sepBy (parse format) (parse separator),
   serialize = \xs-> mconcat <$> sequenceA (List.intersperse (serialize separator ()) $ serialize format <$> xs)}

pair :: (Applicative m, Applicative n, Semigroup s) => Format m n s a -> Format m n s b -> Format m n s (a, b)
-- | Combines two formats into a format for the pair of their values.
--
-- >>> testParse (pair char char) "abc"
-- Right [(('a','b'),"c")]
pair f g = Format{
   parse = (,) <$> parse f <*> parse g,
   serialize = \(a, b)-> Applicative.liftA2 (<>) (serialize f a) (serialize g b)}

deppair :: (Monad m, Applicative n, Semigroup s) => Format m n s a -> (a -> Format m n s b) -> Format m n s (a, b)
-- | Combines two formats, where the second format depends on the first value, into a format for the pair of their
-- values.  Similar to '>>=' except 'Format' is no 'Functor' let alone 'Monad'.
--
-- >>> testParse (deppair char (\c-> satisfy (==c) char)) "abc"
-- Left "encountered 'b'"
-- >>> testParse (deppair char (\c-> satisfy (==c) char)) "aac"
-- Right [(('a','a'),"c")]
deppair f g = Format{
   parse = parse f >>= \a-> parse (g a) >>= \b-> return (a, b),
   serialize = \(a, b)-> Applicative.liftA2 (<>) (serialize f a) (serialize (g a) b)}

empty :: (Alternative m, Alternative n) => Format m n s a
-- | Same as the usual 'Applicative.empty' except a 'Format' is no 'Functor', let alone 'Alternative'.
empty = Format{
   parse = Applicative.empty,
   serialize = const Applicative.empty}

infixr 0 <?>
(<?>) :: (Parser.Parsing m, AlternativeFail n) => Format m n s a -> String -> Format m n s a
-- | Name a format to improve error messages.
--
-- >>> testParse (takeCharsWhile1 isDigit <?> "a number") "abc"
-- Left "expected a number, encountered 'a'"
-- >>> testSerialize (takeCharsWhile1 isDigit <?> "a number") "abc"
-- Left "expected a number, encountered \"abc\""
f <?> name = Format{
   parse = parse f Parser.<?> name,
   serialize = expectedName name . serialize f}

mfix :: MonadFix m => (a -> Format m n s a) -> Format m n s a
-- | Same as the usual 'Control.Monad.Fix.mfix' except a 'Format' is no 'Functor', let alone 'Monad'.
mfix f = Format{
   parse = Monad.Fix.mfix (parse . f),
   serialize = \a-> serialize (f a) a}

-- | Attempts to 'parse' the given input with the format with a constrained type, returns either a failure message or
-- a list of successes.
testParse :: Monoid s => Format (Incremental.Parser Symmetric s) (Either Error) s a -> s -> Either String [(a, s)]
testParse format input = fst <$> Incremental.inspect (Incremental.feedEof $ Incremental.feed input $ parse format)

-- | A less polymorphic wrapper around 'serialize' useful for testing
testSerialize :: Format (Incremental.Parser Symmetric s) (Either Error) s a -> a -> Either String s
testSerialize format = either (Left . errorString) Right . serialize format

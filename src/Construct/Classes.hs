{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeFamilies,
             TypeSynonymInstances, UndecidableInstances #-}

-- | The only good reason to import this module is if you intend to add another instance of the classes it exports.

module Construct.Classes where

import qualified Rank2
import qualified Text.ParserCombinators.Incremental as Incremental

import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (void)
import Data.String (IsString (fromString))
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Text.Parser.Char (CharParsing)
import Text.Parser.Combinators (count, eof, notFollowedBy, try, unexpected)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import qualified Text.Parser.Char as Char

import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Null as Null
import qualified Data.Monoid.Textual as Textual
import qualified Data.Semigroup.Cancellative as Cancellative
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Textual (TextualMonoid)
import Data.Semigroup.Cancellative (LeftReductive)

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text

import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.Char8
import qualified Data.Attoparsec.Text as Attoparsec.Text

import Prelude hiding (take, takeWhile)

-- | Subclass of 'Alternative' that carries an error message in case of failure
class Alternative m => AlternativeFail m where
   -- | Equivalent to 'empty' except it takes an error message it may carry or drop on the floor. The grammatical form
   --  of the argument be a noun representing the unexpected value.
   failure :: String -> m a
   -- | Sets or modifies the expected value.
   expectedName :: String -> m a -> m a

   failure = const empty
   expectedName = const id

-- | Methods for parsing factorial monoid inputs
class LookAheadParsing m => InputParsing m where
   type ParserInput m
   -- | Always sucessful parser that returns the remaining input without consuming it.
   getInput :: m (ParserInput m)

   -- | A parser that accepts any single atomic prefix of the input stream.
   -- > anyToken == satisfy (const True)
   -- > anyToken == take 1
   anyToken :: m (ParserInput m)
   -- | A parser that accepts exactly the given number of input atoms.
   take :: Int -> m (ParserInput m)
   -- | A parser that accepts an input atom only if it satisfies the given predicate.
   satisfy :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | A parser that succeeds exactly when satisfy doesn't, equivalent to
   -- 'Text.Parser.Combinators.notFollowedBy' @. satisfy@
   notSatisfy :: (ParserInput m -> Bool) -> m ()

   -- | A stateful scanner. The predicate modifies a state argument, and each transformed state is passed to successive
   -- invocations of the predicate on each token of the input until one returns 'Nothing' or the input ends.
   --
   -- This parser does not fail.  It will return an empty string if the predicate returns 'Nothing' on the first
   -- character.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'many', because such parsers
   -- loop until a failure occurs.  Careless use will thus result in an infinite loop.
   scan :: state -> (state -> ParserInput m -> Maybe state) -> m (ParserInput m)
   -- | A parser that consumes and returns the given prefix of the input.
   string :: ParserInput m -> m (ParserInput m)

   -- | A parser accepting the longest sequence of input atoms that match the given predicate; an optimized version of
   -- 'concatMany . satisfy'.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'many', because such parsers
   -- loop until a failure occurs.  Careless use will thus result in an infinite loop.
   takeWhile :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | A parser accepting the longest non-empty sequence of input atoms that match the given predicate; an optimized
   -- version of 'concatSome . satisfy'.
   takeWhile1 :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | Zero or more argument occurrences like 'many', with concatenated monoidal results.
   concatMany :: Monoid a => m a -> m a

   anyToken = take 1
   notSatisfy predicate = try (void $ satisfy $ not . predicate) <|> eof
   default concatMany :: (Monoid a, Alternative m) => m a -> m a
   concatMany p = go
      where go = mappend <$> try p <*> go <|> pure mempty

   default string :: (Monad m, LeftReductive (ParserInput m), FactorialMonoid (ParserInput m), Show (ParserInput m))
                  => ParserInput m -> m (ParserInput m)
   string s = do i <- getInput
                 if s `Cancellative.isPrefixOf` i
                    then take (Factorial.length s)
                    else unexpected ("string " <> show s)
   default scan :: (Monad m, FactorialMonoid (ParserInput m)) =>
                   state -> (state -> ParserInput m -> Maybe state) -> m (ParserInput m)
   scan state f = do i <- getInput
                     let (prefix, _suffix, _state) = Factorial.spanMaybe' state f i
                     take (Factorial.length prefix)
   default takeWhile :: (Monad m, FactorialMonoid (ParserInput m)) => (ParserInput m -> Bool) -> m (ParserInput m)
   takeWhile predicate = do i <- getInput
                            take (Factorial.length $ Factorial.takeWhile predicate i)
   default takeWhile1 :: (Monad m, FactorialMonoid (ParserInput m)) => (ParserInput m -> Bool) -> m (ParserInput m)
   takeWhile1 predicate = do x <- takeWhile predicate
                             if Null.null x then unexpected "takeWhile1" else pure x
   {-# INLINE concatMany #-}

-- | Methods for parsing textual monoid inputs
class (CharParsing m, InputParsing m) => InputCharParsing m where
   -- | Specialization of 'satisfy' on textual inputs, accepting an input character only if it satisfies the given
   -- predicate, and returning the input atom that represents the character. Equivalent to @fmap singleton
   -- . Char.satisfy@
   satisfyCharInput :: (Char -> Bool) -> m (ParserInput m)
   -- | A parser that succeeds exactly when satisfy doesn't, equivalent to @notFollowedBy . Char.satisfy@
   notSatisfyChar :: (Char -> Bool) -> m ()

   -- | Stateful scanner like `scan`, but specialized for 'TextualMonoid' inputs.
   scanChars :: state -> (state -> Char -> Maybe state) -> m (ParserInput m)

   -- | Specialization of 'takeWhile' on 'TextualMonoid' inputs, accepting the longest sequence of input characters that
   -- match the given predicate; an optimized version of @fmap fromString  . many . Char.satisfy@.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'many', because such parsers
   -- loop until a failure occurs.  Careless use will thus result in an infinite loop.
   takeCharsWhile :: (Char -> Bool) -> m (ParserInput m)
   -- | Specialization of 'takeWhile1' on 'TextualMonoid' inputs, accepting the longest sequence of input characters
   -- that match the given predicate; an optimized version of @fmap fromString  . some . Char.satisfy@.
   takeCharsWhile1 :: (Char -> Bool) -> m (ParserInput m)

   default satisfyCharInput :: IsString (ParserInput m) => (Char -> Bool) -> m (ParserInput m)
   satisfyCharInput = fmap (fromString . (:[])) . Char.satisfy
   notSatisfyChar = notFollowedBy . Char.satisfy
   default scanChars :: (Monad m, TextualMonoid (ParserInput m)) =>
                        state -> (state -> Char -> Maybe state) -> m (ParserInput m)
   scanChars state f = do i <- getInput
                          let (prefix, _suffix, _state) = Textual.spanMaybe' state (const $ const Nothing) f i
                          take (Factorial.length prefix)

   default takeCharsWhile :: (Monad m, TextualMonoid (ParserInput m)) => (Char -> Bool) -> m (ParserInput m)
   takeCharsWhile predicate = do i <- getInput
                                 take (Factorial.length $ Textual.takeWhile_ False predicate i)
   default takeCharsWhile1 :: (Monad m, TextualMonoid (ParserInput m)) => (Char -> Bool) -> m (ParserInput m)
   takeCharsWhile1 predicate = do x <- takeCharsWhile predicate
                                  if Null.null x then unexpected "takeCharsWhile1" else pure x

-- | A subclass of 'InputParsing' for parsers that can switch the input stream type
class InputMappableParsing m where
   -- | Converts a parser accepting one input stream type to another. The functions @forth@ and @back@ must be inverses of
   -- each other and they must distribute through '<>':
   --
   -- > f (s1 <> s2) == f s1 <> f s2
   mapParserInput :: (InputParsing (m s), s ~ ParserInput (m s), Monoid s, Monoid s') =>
                     (s -> s') -> (s' -> s) -> m s a -> m s' a
   -- | Converts a parser accepting one input stream type to another just like 'mapParserInput', except the argument
   -- functions can return @Nothing@ to indicate they need more input.
   
   mapMaybeParserInput :: (InputParsing (m s), s ~ ParserInput (m s), Monoid s, Monoid s') =>
                          (s -> Maybe s') -> (s' -> Maybe s) -> m s a -> m s' a

-- | A subclass of 'MonadFix' for monads that can fix a function that handles higher-kinded data
class Monad m => FixTraversable m where
   -- | This specialized form of 'Rank2.traverse' can be used inside 'mfix'.
   fixSequence :: (Rank2.Traversable g, Applicative n) => g m -> m (g n)
   fixSequence = Rank2.traverse (pure <$>)

------------------------------------------------------------
--                       Instances
------------------------------------------------------------

data Error = Error [String] (Maybe String) deriving (Eq, Show)

instance Semigroup Error where
   Error expected1 encountered1 <> Error expected2 encountered2 =
      Error (expected1 <> expected2) (maybe encountered2 Just encountered1)

instance AlternativeFail Maybe

instance AlternativeFail []

instance {-# OVERLAPS #-} Alternative (Either Error) where
   empty = Left (Error [] Nothing)
   Right a <|> _ = Right a
   _ <|> Right a = Right a
   Left e1 <|> Left e2 = Left (e1 <> e2)

instance AlternativeFail (Either Error) where
   failure encountered = Left (Error [] (Just encountered))
   expectedName expected (Left (Error _ encountered)) = Left (Error [expected] encountered)
   expectedName _ success = success

errorString :: Error -> String
errorString (Error ex Nothing) = maybe "" ("expected " <>) (concatExpected ex)
errorString (Error [] (Just en)) = "encountered " <> en
errorString (Error ex (Just en)) = maybe "" ("expected " <>) (concatExpected ex) <> ", encountered " <> en

concatExpected :: [String] -> Maybe String
concatExpected [] = Nothing
concatExpected [e] = Just e
concatExpected [e1, e2] = Just (e1 <> " or " <> e2)
concatExpected (e:es) = Just (oxfordComma e es)

oxfordComma :: String -> [String] -> String
oxfordComma e [] = "or " <> e
oxfordComma e (e':es) = e <> ", " <> oxfordComma e' es

instance InputParsing ReadP where
   type ParserInput ReadP = String
   getInput = ReadP.look
   take n = count n ReadP.get
   anyToken = pure <$> ReadP.get
   satisfy predicate = pure <$> ReadP.satisfy (predicate . pure)
   string = ReadP.string

instance InputCharParsing ReadP where
   satisfyCharInput predicate = pure <$> ReadP.satisfy predicate

instance InputParsing Attoparsec.Parser where
   type ParserInput Attoparsec.Parser = ByteString
   getInput = lookAhead Attoparsec.takeByteString
   anyToken = Attoparsec.take 1
   take = Attoparsec.take
   satisfy predicate = Attoparsec.satisfyWith ByteString.singleton predicate
   string = Attoparsec.string

instance InputCharParsing Attoparsec.Parser where
   satisfyCharInput predicate = ByteString.Char8.singleton <$> Attoparsec.Char8.satisfy predicate
   scanChars = Attoparsec.Char8.scan
   takeCharsWhile = Attoparsec.Char8.takeWhile
   takeCharsWhile1 = Attoparsec.Char8.takeWhile1

instance InputParsing Attoparsec.Text.Parser where
   type ParserInput Attoparsec.Text.Parser = Text
   getInput = lookAhead Attoparsec.Text.takeText
   anyToken = Attoparsec.Text.take 1
   take = Attoparsec.Text.take
   satisfy predicate = Attoparsec.Text.satisfyWith Text.singleton predicate
   string = Attoparsec.Text.string

instance InputCharParsing Attoparsec.Text.Parser where
   satisfyCharInput predicate = Text.singleton <$> Attoparsec.Text.satisfy predicate
   scanChars = Attoparsec.Text.scan
   takeCharsWhile = Attoparsec.Text.takeWhile
   takeCharsWhile1 = Attoparsec.Text.takeWhile1

instance (FactorialMonoid s, Cancellative.LeftReductive s, LookAheadParsing (Incremental.Parser t s)) =>
         InputParsing (Incremental.Parser t s) where
   type ParserInput (Incremental.Parser t s) = s
   getInput = lookAhead Incremental.acceptAll
   anyToken = Incremental.anyToken
   take n = Incremental.count n Incremental.anyToken
   satisfy = Incremental.satisfy
   string = Incremental.string
   takeWhile = Incremental.takeWhile
   takeWhile1 = Incremental.takeWhile1
   concatMany = Incremental.concatMany

instance (TextualMonoid s, Cancellative.LeftReductive s, LookAheadParsing (Incremental.Parser t s)) =>
         InputCharParsing (Incremental.Parser t s) where
   satisfyCharInput = Incremental.satisfyChar
   takeCharsWhile = Incremental.takeCharsWhile
   takeCharsWhile1 = Incremental.takeCharsWhile1

instance FixTraversable Attoparsec.Parser

instance Monoid s => FixTraversable (Incremental.Parser t s) where
   fixSequence = Incremental.record

instance InputMappableParsing (Incremental.Parser t) where
   mapParserInput = Incremental.mapInput
   mapMaybeParserInput = Incremental.mapMaybeInput


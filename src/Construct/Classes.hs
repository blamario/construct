{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeFamilies,
             TypeOperators, TypeSynonymInstances, UndecidableInstances #-}

-- | The only good reason to import this module is if you intend to add another instance of the classes it exports.

module Construct.Classes where

import qualified Rank2
import qualified Text.ParserCombinators.Incremental as Incremental

import Control.Applicative (Alternative ((<|>), empty))
import qualified Data.Attoparsec.ByteString as Attoparsec
import Text.Parser.Input (InputParsing (ParserInput))

-- | Subclass of 'Alternative' that carries an error message in case of failure
class Alternative m => AlternativeFail m where
   -- | Equivalent to 'empty' except it takes an error message it may carry or drop on the floor. The grammatical form
   --  of the argument should be a noun representing the unexpected value.
   failure :: String -> m a
   -- | Sets or modifies the expected value.
   expectedName :: String -> m a -> m a

   failure = const empty
   expectedName = const id

-- | A subclass of 'InputParsing' for parsers that can switch the input stream type
class InputMappableParsing m where
   -- | Converts a parser accepting one input stream type to another. The functions @forth@ and @back@ must be
   -- inverses of each other and they must distribute through '<>':
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

instance FixTraversable Attoparsec.Parser

instance Monoid s => FixTraversable (Incremental.Parser t s) where
   fixSequence = Incremental.record

instance InputMappableParsing (Incremental.Parser t) where
   mapParserInput = Incremental.mapInput
   mapMaybeParserInput = Incremental.mapMaybeInput


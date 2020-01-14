{-# LANGUAGE FlexibleInstances, GADTs #-}

module Construct.Classes where

import Control.Monad.Fix (MonadFix)
import qualified Rank2
import Text.Grampa (InputParsing(ParserInput, anyToken, string), InputCharParsing)
import qualified Text.ParserCombinators.Incremental as Incremental
import Text.ParserCombinators.Incremental.LeftBiasedLocal (LeftBiasedLocal)

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
class MonadFix m => FixTraversable m where
   -- | This specialized form of 'Rank2.traverse' can be used inside 'mfix'.
   --
   -- > mfix (fixSequence . f) == fix (fixSequence . f =<<)
   fixSequence :: (Rank2.Traversable g, Applicative n) => g m -> m (g n)
   fixSequence = Rank2.traverse (pure <$>)

instance Monoid s => FixTraversable (Incremental.Parser t s) where
   fixSequence = Incremental.record

instance InputMappableParsing (Incremental.Parser LeftBiasedLocal) where
   mapParserInput = Incremental.mapInput
   mapMaybeParserInput = Incremental.mapMaybeInput


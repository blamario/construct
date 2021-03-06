{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}

module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showHex)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (combine)
import qualified Text.ParserCombinators.Incremental as Incremental
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser)
import qualified Data.Attoparsec.ByteString as Atto
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, assertEqual, testCase)

import Construct
import qualified MBR
import qualified TAR
import qualified URI
import qualified WMF

import Debug.Trace

data TestFormat = forall f. TestFormat (Format (Parser ByteString) Maybe ByteString (f Identity))
                | forall f. LineFormat (Format (Parser ByteString) Maybe ByteString (f Identity))
                | forall f. AttoLineFormat (Format Atto.Parser Maybe ByteString (f Identity))
                | forall f. AttoFormat (Format Atto.Parser Maybe ByteString (f Identity))

main = exampleTree "" "test/examples" >>= defaultMain . testGroup "examples"

exampleTree :: FilePath -> FilePath -> IO [TestTree]
exampleTree ancestry path =
   do let fullPath = combine ancestry path
      isDir <- doesDirectoryExist fullPath
      if isDir
         then (:[]) . testGroup path . concat <$> (listDirectory fullPath >>= mapM (exampleTree fullPath))
         else do blob <- ByteString.readFile fullPath
                 let format
                        | ".mbr"  `isSuffixOf` path = TestFormat MBR.format
                        | ".tar"  `isSuffixOf` path = AttoFormat TAR.archive
                        | ".uris" `isSuffixOf` path = AttoLineFormat URI.uriReference
                        | ".wmf"  `isSuffixOf` path = TestFormat WMF.fileFormat
                     roundTrip f t
                        | ByteString.null t = Just t
                        | [(structure, remainder)] <- Incremental.completeResults (Incremental.feedEof $
                                                                                   Incremental.feed t $ parse f),
                          ByteString.null remainder = serialize f structure
                        | otherwise = Nothing
                     attoRoundTrip f t
                        | ByteString.null t = Just t
                        | Atto.Done remainder structure <- Atto.parse (parse f) t, ByteString.null remainder =
                             serialize f structure
                        | Atto.Partial i <- Atto.parse (parse f) t,
                          Atto.Done remainder structure <- i mempty, ByteString.null remainder = serialize f structure
                        | otherwise = Nothing
                     Just blob'
                        | TestFormat f <- format,
                          [(structure, remainder)] <- Incremental.completeResults (Incremental.feedEof $
                                                                                   Incremental.feed blob $ parse f) =
                             (<> remainder) <$> serialize f structure
                        | LineFormat f <- format = Just (mconcat
                                                         $ map ((<> "\n") . fromMaybe "???" . roundTrip f)
                                                         $ ASCII.lines blob)
                        | AttoLineFormat f <- format = Just (mconcat
                                                             $ map ((<> "\n") . fromMaybe "???" . attoRoundTrip f)
                                                             $ ASCII.lines blob)
                        | AttoFormat f <- format,
                          Atto.Done remainder structure <- Atto.parse (parse f) blob =
                             (<> remainder) <$> serialize f structure
                        | AttoFormat f <- format, Atto.Partial i <- Atto.parse (parse f) blob,
                          Atto.Done remainder structure <- i mempty =
                             (<> remainder) <$> serialize f structure
                 return . (:[]) . testCase path $ assertEqual "round-trip" (hex format blob) (hex format blob')

hex :: TestFormat -> ByteString -> String
hex LineFormat{} = ASCII.unpack
hex AttoLineFormat{} = ASCII.unpack
hex _ = ByteString.foldr (pad . flip showHex "") ""
   where pad [x] s = ['0', x] ++ s
         pad [x, y] s = [x, y] ++ s

{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}

module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
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

data TestFormat = forall f. TestFormat (Format (Parser ByteString) Maybe ByteString (f Identity))
                | forall f. LineFormat (Format (Parser Text) Maybe Text (f Identity))
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
                        | ".uris" `isSuffixOf` path = LineFormat URI.uriReference
                        | ".wmf"  `isSuffixOf` path = TestFormat WMF.fileFormat
                     textLines = Text.lines (decodeUtf8 blob)
                     roundTrip f t
                        | Text.null t = Just t
                        | [(structure, remainder)] <- Incremental.completeResults (Incremental.feedEof $
                                                                                   Incremental.feed t $ parse f),
                          Text.null remainder = serialize f structure
                        | otherwise = Nothing
                     Just blob'
                        | TestFormat f <- format,
                          [(structure, remainder)] <- Incremental.completeResults (Incremental.feedEof $
                                                                                   Incremental.feed blob $ parse f) =
                             (<> remainder) <$> serialize f structure
                        | LineFormat f <- format = Just (encodeUtf8 $ mconcat
                                                         $ map ((<> "\n") . fromMaybe "???" . roundTrip f) textLines)
                        | AttoFormat f <- format,
                          Atto.Done remainder structure <- Atto.parse (parse f) blob =
                             (<> remainder) <$> serialize f structure
                        | AttoFormat f <- format, Atto.Partial i <- Atto.parse (parse f) blob,
                          Atto.Done remainder structure <- i mempty =
                             (<> remainder) <$> serialize f structure
                 return . (:[]) . testCase path $ assertEqual "round-trip" (hex format blob) (hex format blob')

hex :: TestFormat -> ByteString -> String
hex LineFormat{} = Char8.unpack
hex _ = ByteString.foldr (pad . flip showHex "") ""
   where pad [x] s = ['0', x] ++ s
         pad [x, y] s = [x, y] ++ s

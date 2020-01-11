{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}

module Main where

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity)
import Data.List (isSuffixOf)
import Numeric (showHex)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (combine)
import qualified Text.ParserCombinators.Incremental as Incremental
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, assertEqual, testCase)

import Construct
import qualified MBR
import qualified WMF

data TestFormat = forall f. TestFormat (Format (Parser ByteString) Maybe ByteString (f Identity))

main = exampleTree "" "test/examples" >>= defaultMain . testGroup "examples"

exampleTree :: FilePath -> FilePath -> IO [TestTree]
exampleTree ancestry path =
   do let fullPath = combine ancestry path
      isDir <- doesDirectoryExist fullPath
      if isDir
         then (:[]) . testGroup path . concat <$> (listDirectory fullPath >>= mapM (exampleTree fullPath))
         else do blob <- ByteString.readFile fullPath
                 let format
                        | ".wmf" `isSuffixOf` path = TestFormat WMF.fileFormat
                        | ".mbr" `isSuffixOf` path = TestFormat MBR.format
                     Just blob'
                        | TestFormat f <- format,
                          [(structure, remainder)] <- Incremental.completeResults (Incremental.feedEof $
                                                                                   Incremental.feed blob $ parse f) =
                             (<> remainder) <$> serialize f structure
                 return . (:[]) . testCase path $ assertEqual "round-trip" (hex blob) (hex blob')

hex :: ByteString -> String
hex = ByteString.foldr (pad . flip showHex "") ""
   where pad [x] s = ['0', x] ++ s
         pad [x, y] s = [x, y] ++ s

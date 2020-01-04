{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (isSuffixOf)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Numeric (showHex)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (combine)
import qualified Text.ParserCombinators.Incremental as Incremental
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, assertEqual, testCase)

import Construct
import qualified WMF

main = exampleTree "" "test/examples" >>= defaultMain . testGroup "Oberon"

exampleTree :: FilePath -> FilePath -> IO [TestTree]
exampleTree ancestry path =
   do let fullPath = combine ancestry path
      isDir <- doesDirectoryExist fullPath
      if isDir
         then (:[]) . testGroup path . concat <$> (listDirectory fullPath >>= mapM (exampleTree fullPath))
         else if ".wmf" `isSuffixOf` path
              then return . (:[]) . testCase path $
                   do blob <- ByteString.readFile fullPath
                      let [(structure, "")] = Incremental.completeResults (Incremental.feedEof $ Incremental.feed blob
                                                                           $ parse WMF.fileFormat)
                          Just blob' = serialize WMF.fileFormat structure
                      assertEqual "round-trip" (hex blob) (hex blob')
              else return []

hex :: ByteString -> String
hex = ByteString.foldr (pad . flip showHex "") ""
   where pad [x] s = ['0', x] ++ s
         pad [x, y] s = [x, y] ++ s

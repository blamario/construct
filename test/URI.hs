{-# LANGUAGE OverloadedStrings #-}

module URI where

import Data.ByteString (ByteString)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Word (Word8)
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import Data.String (fromString)
import Numeric.Natural (Natural)
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser, completeResults, feed, feedEof)

import Construct
import OrphanInstances

import Prelude hiding ((<$), (<*), (*>), take, takeWhile)

main = print (completeResults $ feedEof $ feed "ftp://ftp.is.co.za/rfc/rfc1808.txt" $ parse uriScheme)

uriScheme :: Format (Parser ByteString) Maybe ByteString ByteString
uriScheme = mapValue (uncurry (<>)) (Factorial.splitAt 1) $
            pair (satisfy (any isAlpha . Textual.characterPrefix) $ take 1) (takeCharsWhile isAlphaNum)

ipV4address :: Format (Parser ByteString) Maybe ByteString [Word8]
ipV4address = satisfy ((== 4) . length) (decOctet `sepBy` literal ".")
   where decOctet = mapValue fromIntegral fromIntegral $
                    satisfy (< 256) $ mapDec $
                    takeCharsWhile1 isDigit

mapDec :: Format (Parser ByteString) Maybe ByteString ByteString -> Format (Parser ByteString) Maybe ByteString Natural
mapDec = mapValue (read . Textual.toString (error . show)) (fromString . show)

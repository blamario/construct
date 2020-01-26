{-# LANGUAGE OverloadedStrings, TupleSections #-}

module URI where

import Data.Bits ((.|.), (.&.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isHexDigit, chr, ord, toUpper)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.List as List
import Data.Maybe (isNothing, maybe)
import Data.Monoid.Textual (TextualMonoid)
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import Numeric (readHex, showHex)
import Numeric.Natural (Natural)
import qualified Rank2.TH
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

ipV6address :: Format (Parser ByteString) Maybe ByteString [Word16]
ipV6address = satisfy ((== 8) . length) $
              mapValue fill shorten ipV6addressShort
   where fill :: [Maybe Word16] -> [Word16]
         shorten :: [Word16] -> [Maybe Word16]
         fill words = concatMap (maybe (replicate (9 - length words) 0) (:[])) words
         shorten [] = []
         shorten (0:0:words) = Nothing : map Just (dropWhile (== 0) words)
         shorten (word:words) = Just word : shorten words

ipV6addressShort :: Format (Parser ByteString) Maybe ByteString [Maybe Word16]
ipV6addressShort = satisfy zeroOrOneNothings $
                   mapValue (uncurry (++)) (, []) $
                   pair (optional h16 `sepBy` literal ":")
                        (literal ":" *> mapValue v4tov6 v6tov4 ipV4address <|> [] <$ literal "")
   where v4tov6 [] = []
         v4tov6 (byte1:byte2:bytes) = Just (shift (fromIntegral byte1) 8 .|. fromIntegral byte2) : v4tov6 bytes
         v4tov6 _ = error "odd number of ipv4 bytes"
         v6tov4 [] = []
         v6tov4 (Just word:words) = fromIntegral (shift word $ negate 8) : fromIntegral (word .&. 0xFF) : v6tov4 words
         v6tov4 (Nothing:words) = 0 : 0 : v6tov4 words
         zeroOrOneNothings words = elisionCount == 0 && length words == 8 || elisionCount == 1 && length words < 7
            where elisionCount = length (filter isNothing words)
         h16 = mapHex $ satisfy ((<5) . Factorial.length) $ takeCharsWhile1 isHexDigit

mapHex :: (Integral n, Show n) =>
          Format (Parser ByteString) Maybe ByteString ByteString -> Format (Parser ByteString) Maybe ByteString n
mapDec :: Format (Parser ByteString) Maybe ByteString ByteString -> Format (Parser ByteString) Maybe ByteString Natural
mapDec = mapValue (read . Textual.toString (error . show)) (fromString . show)
mapHex = mapValue (fst . head . readHex . Textual.toString (error . show)) (fromString . flip showHex "")

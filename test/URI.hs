{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving,
             TemplateHaskell, TupleSections, TypeApplications #-}

module URI where

import Data.Bits ((.|.), (.&.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import Data.Char (isAlpha, isAscii, isDigit, isHexDigit, chr, ord)
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
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser)

import Construct

import Prelude hiding ((<$), (<*), (*>), take, takeWhile)

data UriReference t f = UriReference{
   scheme    :: f (Maybe t),
   authority :: f (Maybe (Authority t Identity)),
   path      :: f [t],
   query     :: f (Maybe t),
   fragment  :: f (Maybe t)
   }

data Authority t f = Authority{
   user :: f (Maybe t),
   host :: f (HostName t),
   port :: f (Maybe Word16)
   }

data HostName t = IPv4address{getIPv4address :: [Word8]}
                | IPv6address{getIPv6address :: [Word16]}
                | IPvFuture {version :: Word,
                             address :: t}
                | RegisteredName{getRegisteredName :: t}

uriReference :: (Show t, TextualMonoid t) => Format (Parser t) Maybe t (UriReference t Identity)
uriReference = record UriReference{
   scheme = optional (mapValue (uncurry (<>)) (Factorial.splitAt 1) $
                      pair (satisfy (any alpha . Textual.characterPrefix) $ take 1)
                           (takeCharsWhile schemeChar)
                      <* literal ":"),
   authority = optional $ record Authority{
        user = optional (takeCharsWhile userChar <* literal "@"),
        host = hostName,
        port = optional (mapValue fromIntegral fromIntegral $ satisfy (< 65536) $
                         literal ":" *> mapDec (takeCharsWhile1 digit))},  -- port = *DIGIT in spec?
   path = encodedCharSequence pathChar `sepBy` literal "/",
   query = optional (literal "*" *> encodedCharSequence queryChar),
   fragment = optional (literal "#" *> encodedCharSequence fragmentChar)
   }

hostName :: (Show t, TextualMonoid t) => Format (Parser t) Maybe t (HostName t)
hostName = mapValue IPv4address getIPv4address ipV4address
           <|> literal "["
               *> (mapValue IPv6address getIPv6address ipV6address
                    <|> mapValue (uncurry IPvFuture) (\ipf-> (version ipf, address ipf))
                                 (pair (literal "v" *> mapHex (takeCharsWhile1 hexDigit))
                                       (literal "." *> takeCharsWhile1 ipFutureChar)))
               <* literal "]"
           <|> mapValue RegisteredName getRegisteredName (encodedCharSequence hostChar)

ipV4address :: forall t. (Show t, TextualMonoid t) => Format (Parser t) Maybe t [Word8]
ipV4address = satisfy ((== 4) . length) (decOctet `sepBy` literal ".")
   where decOctet = mapValue fromIntegral fromIntegral $
                    satisfy (< 256) $ mapDec $
                    takeCharsWhile1 digit

ipV6address :: (Show t, TextualMonoid t) => Format (Parser t) Maybe t [Word16]
ipV6address = satisfy ((== 8) . length) $
              mapValue fill shorten ipV6addressShort
   where fill :: [Maybe Word16] -> [Word16]
         shorten :: [Word16] -> [Maybe Word16]
         fill words = concatMap (maybe (replicate (8 - length words) 0) (:[])) words
         shorten [] = []
         shorten (0:0:words) = Nothing : map Just (dropWhile (== 0) words)
         shorten (word:words) = Just word : shorten words

ipV6addressShort :: (Show t, TextualMonoid t) => Format (Parser t) Maybe t [Maybe Word16]
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
         h16 = mapHex $ satisfy ((<5) . Factorial.length) $ takeCharsWhile1 hexDigit

mapHex :: (Show t, TextualMonoid t, Integral n, Show n) => Format (Parser t) Maybe t t -> Format (Parser t) Maybe t n
mapDec :: (Show t, TextualMonoid t) => Format (Parser t) Maybe t t -> Format (Parser t) Maybe t Natural
mapDec = mapValue (read . Textual.toString (error . show)) (fromString . show)
mapHex = mapValue (fst . head . readHex . Textual.toString (error . show)) (fromString . flip showHex "")

alpha, digit, hexDigit, schemeChar, hostChar, userChar, ipFutureChar,
   pathChar, queryChar, fragmentChar, unreserved, subDelim :: Char -> Bool

alpha c      = isAlpha c && isAscii c
digit c      = isAlpha c && isDigit c
hexDigit c   = isAlpha c && isHexDigit c
schemeChar c = (isAlpha c || isDigit c) && isAscii c || elem @[] c "+-."
ipFutureChar = userChar
hostChar c   = unreserved c || subDelim c
userChar c   = unreserved c || subDelim c || c == ':'
pathChar c   = unreserved c || subDelim c || c == ':' || c == '@'
queryChar c  = pathChar c || c == '/' || c == '?'
fragmentChar = queryChar
subDelim c   = elem @[] c "!$&'()*+,;="
unreserved c = isAscii c && (isAlpha c || isDigit c || elem @[] c "-._~")

encodedCharSequence :: forall t. (Show t, TextualMonoid t) => (Char -> Bool) -> Format (Parser t) Maybe t t
encodedCharSequence predicate = mapValue concatSequence splitSequence $
                                many (takeCharsWhile predicate <+> percentEncoded)
   where concatSequence :: [Either t Char] -> t
         splitSequence :: t -> [Either t Char]
         percentEncoded :: Format (Parser t) Maybe t Char
         concatSequence = mconcat . map (either id Textual.singleton)
         splitSequence s = case Textual.splitCharacterPrefix s
            of Just ('#', rest)
                  | Just (hex1, rest1) <- Textual.splitCharacterPrefix rest,
                    Just (hex2, rest2) <- Textual.splitCharacterPrefix rest1 ->
                       Right (hexChar [hex1, hex2]) : splitSequence rest2
                  | otherwise -> []
               Just (c, _)
                  | predicate c, (prefix, rest) <- Textual.span_ False predicate s -> Left prefix : splitSequence rest
               _ -> []
         percentEncoded = mapValue hexChar (padLeft . (`showHex` "") . ord) $
                          literal "#" *> count 2 (satisfy hexDigit char)
         hexChar = chr . fst . head . readHex
         padLeft [c] = ['0', c]
         padLeft cs = cs

$(Rank2.TH.deriveAll ''UriReference)
$(Rank2.TH.deriveAll ''Authority)

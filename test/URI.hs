{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving,
             TemplateHaskell, TupleSections, TypeApplications #-}

module URI where

import Data.Bits ((.|.), (.&.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import Data.Char (isAlpha, isAscii, isDigit, isHexDigit, chr, ord, toUpper)
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
import OrphanInstances

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

data HostName t = IPv4address [Word8]
                | IPv6address [Word16]
                | IPvFuture Word t
                | RegisteredName t
                deriving (Eq, Read, Show)

deriving instance Show t => Show (UriReference t Identity)
deriving instance Show t => Show (Authority t Identity)

uriReference :: Format (Parser ByteString) Maybe ByteString (UriReference ByteString Identity)
uriReference = record UriReference{
   scheme = optional (uriScheme <* literal ":"),
   authority = optional (literal "//" *> uriAuthority),
   path = encodedCharSequence pathChar `sepBy` literal "/",
   query = optional (literal "?" *> encodedCharSequence queryChar),
   fragment = optional (literal "#" *> encodedCharSequence fragmentChar)
   }

uriAuthority :: Format (Parser ByteString) Maybe ByteString (Authority ByteString Identity)
uriAuthority = record Authority{
        user = optional (takeCharsWhile userChar <* literal "@"),
        host = hostName,
        port = optional (mapValue fromIntegral fromIntegral $
                         satisfy (< 65536) $ literal ":" *> mapDec (takeCharsWhile1 digit))}  -- port = *DIGIT in spec?

uriScheme :: Format (Parser ByteString) Maybe ByteString ByteString
uriScheme = mapValue (uncurry (<>)) (Factorial.splitAt 1) $
            pair (satisfy (any alpha . Textual.characterPrefix) $ take 1) (takeCharsWhile schemeChar)
                      
hostName :: Format (Parser ByteString) Maybe ByteString (HostName ByteString)
hostName = mapMaybeValue (Just . IPv4address) (\case (IPv4address a)-> Just a; _ -> Nothing) ipV4address
           <|> literal "["
               *> (mapMaybeValue (Just . IPv6address) (\case (IPv6address a)-> Just a; _ -> Nothing) ipV6address
                    <|> mapMaybeValue (Just . uncurry IPvFuture) (\case (IPvFuture v a)-> Just (v, a); _ -> Nothing)
                                      (pair (literal "v" *> mapHex (takeCharsWhile1 hexDigit))
                                            (literal "." *> takeCharsWhile1 ipFutureChar)))
               <* literal "]"
           <|> mapMaybeValue (Just . RegisteredName) (\case (RegisteredName a)-> Just a; _ -> Nothing)
                             (encodedCharSequence hostChar)

ipV4address :: Format (Parser ByteString) Maybe ByteString [Word8]
ipV4address = satisfy ((== 4) . length) (decOctet `sepBy` literal ".")
   where decOctet = mapValue fromIntegral fromIntegral $
                    satisfy (< 256) $ mapDec $
                    takeCharsWhile1 digit

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
         h16 = mapHex $ satisfy ((<5) . Factorial.length) $ takeCharsWhile1 hexDigit

mapHex :: (Integral n, Show n) =>
          Format (Parser ByteString) Maybe ByteString ByteString -> Format (Parser ByteString) Maybe ByteString n
mapDec :: Format (Parser ByteString) Maybe ByteString ByteString -> Format (Parser ByteString) Maybe ByteString Natural
mapDec = mapValue (read . Textual.toString (error . show)) (fromString . show)
mapHex = mapValue (fst . head . readHex . Textual.toString (error . show)) (fromString . flip showHex "")

alpha, digit, hexDigit, schemeChar, hostChar, userChar, ipFutureChar,
   pathChar, queryChar, fragmentChar, unreserved, subDelim :: Char -> Bool

alpha c      = isAlpha c && isAscii c
digit c      = isAlpha c && isDigit c
hexDigit c   = isHexDigit c && isAscii c
schemeChar c = (isAlpha c || isDigit c) && isAscii c || elem @[] c "+-."
ipFutureChar = userChar
hostChar c   = unreserved c || subDelim c
userChar c   = unreserved c || subDelim c || c == ':'
pathChar c   = unreserved c || subDelim c || c == ':' || c == '@'
queryChar c  = pathChar c || c == '/' || c == '?'
fragmentChar = queryChar
subDelim c   = elem @[] c "!$&'()*+,;="
unreserved c = isAscii c && (isAlpha c || isDigit c || elem @[] c "-._~")

encodedCharSequence :: (Char -> Bool) -> Format (Parser ByteString) Maybe ByteString ByteString
encodedCharSequence predicate = mapValue concatSequence splitSequence $
                                many (takeCharsWhile1 predicate <+> percentEncoded)
   where concatSequence :: [Either ByteString Char] -> ByteString
         splitSequence :: ByteString -> [Either ByteString Char]
         percentEncoded :: Format (Parser ByteString) Maybe ByteString Char
         concatSequence = mconcat . map (either id Textual.singleton)
         splitSequence s = case Textual.splitCharacterPrefix s
            of Just (c, t)
                  | predicate c, (prefix, rest) <- Textual.span_ False predicate s -> Left prefix : splitSequence rest
                  | otherwise -> Right c : splitSequence t
               _ -> []
         percentEncoded = mapValue hexChar (padLeft . map toUpper . (`showHex` "") . ord) $
                          literal "%" *> count 2 (satisfy hexDigit char)
         hexChar = chr . fst . head . readHex
         padLeft [c] = ['0', c]
         padLeft cs = cs

$(Rank2.TH.deriveAll ''UriReference)
$(Rank2.TH.deriveAll ''Authority)

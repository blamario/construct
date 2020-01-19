{-# LANGUAGE FlexibleInstances, OverloadedStrings, StandaloneDeriving, TemplateHaskell #-}

module TAR where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import Data.Char (isOctDigit)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Monoid.Textual (TextualMonoid)
import qualified Data.Monoid.Textual
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import Numeric (readOct, showOct)
import qualified Rank2.TH
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser)

import Construct

import Prelude hiding ((<$), (<*), (*>), take, takeWhile)

data Archive f = Archive{files :: f [File Identity]}

data File f = File{
   header  :: f (FileHeader () Identity),
   content :: f ByteString}

data FileType = NormalFile
              | HardLink
              | SymLink
              | CharSpecial
              | BlockSpecial
              | Directory
              | FIFO
              | ContiguousFile
              | ExtendedGlobalHeader
              | ExtendedFileHeader
              | VendorExtension{vendorCode :: Word8}
              | Reserved{reservedCode :: Word8}
              deriving (Eq, Read, Show)

data FileHeader cksum f = FileHeader{
   fileName :: f Text,
   fileMode :: f Word32,
   ownerID  :: f Word32,
   groupID  :: f Word32,
   fileSize :: f Word64,
   modified :: f Word64,
   checksum :: f cksum,
   fileType :: f FileType,
   linked   :: f Text,
   ustar    :: f (Maybe (UStarHeader Identity))}

data UStarHeader f = UStarHeader{
   ownerName   :: f Text,
   groupName   :: f Text,
   deviceMajor :: f (Maybe Word32),
   deviceMinor :: f (Maybe Word32),
   namePrefix  :: f Text}

deriving instance Show (Archive Identity)
deriving instance Show (File Identity)
deriving instance Show (FileHeader () Identity)
deriving instance Show (FileHeader Word32 Identity)
deriving instance Show (UStarHeader Identity)

archive :: Format (Parser ByteString) Maybe ByteString (Archive Identity)
archive = record Archive{files= many file}

file :: Format (Parser ByteString) Maybe ByteString (File Identity)
file = mfix $ \this-> record File{
   header = fileHeader,
   content = ByteString.replicate (((fromIntegral (fileSize <$> header this) + 511) `div` 512) * 512) 0 `padded`
             take (fromIntegral $ fileSize <$> header this)}

fileHeader :: Format (Parser ByteString) Maybe ByteString (FileHeader () Identity)
fileHeader = mapMaybeValue validate calculate (record $ fileHeaderRecord (zeroDelimitedOctal 7 <* value char ' '))
   where validate header
            | header' <- header{checksum= pure ()}, fromIntegral (checksum header) == sumOf header' = Just header'
            | otherwise = Nothing
         calculate header = Just header{checksum= pure $ sumOf header}

fileHeaderRecord :: Format (Parser ByteString) Maybe ByteString cksum
                 -> FileHeader cksum (Format (Parser ByteString) Maybe ByteString)
fileHeaderRecord checksumFormat = FileHeader{
   fileName = zeroDelimitedUtf8 100,
   fileMode = zeroDelimitedOctal 8,
   ownerID  = zeroDelimitedOctal 8,
   groupID  = zeroDelimitedOctal 8,
   fileSize = zeroDelimitedOctal64,
   modified = zeroDelimitedOctal64,
   checksum = checksumFormat,
   fileType = NormalFile <$ value char '0'
              <|> HardLink <$ value char '1'
              <|> SymLink <$ value char '2'
              <|> CharSpecial <$ value char '3'
              <|> BlockSpecial <$ value char '4'
              <|> Directory <$ value char '5'
              <|> FIFO <$ value char '6'
              <|> ContiguousFile <$ value char '7'
              <|> ExtendedGlobalHeader <$ value char 'g'
              <|> ExtendedFileHeader <$ value char 'x'
              <|> mapValue VendorExtension vendorCode byte
              <|> mapValue Reserved reservedCode byte,
   linked = zeroDelimitedUtf8 100,
   ustar = optional ((value (zeroDelimitedUtf8 8) "ustar  " <|> value (zeroDelimitedUtf8 8) "ustar00") *> ustarHeader)}

ustarHeader :: Format (Parser ByteString) Maybe ByteString (UStarHeader Identity)
ustarHeader = record UStarHeader{
   ownerName = zeroDelimitedUtf8 32,
   groupName = zeroDelimitedUtf8 32,
   deviceMajor = optionWithDefault (literal $ ByteString.replicate 8 0) (zeroDelimitedOctal 8),
   deviceMinor = optionWithDefault (literal $ ByteString.replicate 8 0) (zeroDelimitedOctal 8),
   namePrefix = zeroDelimitedUtf8 167 {- 155+12 to pad to 512 -}}

zeroDelimitedUtf8 :: Int -> Format (Parser ByteString) Maybe ByteString Text
zeroDelimitedUtf8 width = mapValue decodeUtf8 encodeUtf8 $
                          ByteString.replicate width 0 `padded1` (takeWhile (/= ByteString.singleton 0))

zeroDelimitedOctal :: Int -> Format (Parser ByteString) Maybe ByteString Word32
zeroDelimitedOctal width = mapValue (fst . head . readOct . ASCII.unpack)
                                    (padLeft '0' (width - 1) . ASCII.pack . flip showOct "") $
                           ByteString.replicate width 0 `padded1` (takeCharsWhile1 isOctDigit)

zeroDelimitedOctal64 :: Format (Parser ByteString) Maybe ByteString Word64
zeroDelimitedOctal64 = mapValue (fst . head . readOct . ASCII.unpack)
                                (padLeft '0' 11 . ASCII.pack . flip showOct "") $
                       ByteString.replicate 12 0 `padded1` (takeCharsWhile1 isOctDigit)

padLeft :: Char -> Int -> ByteString -> ByteString
padLeft filler len bs
   | ByteString.length bs < len = ASCII.replicate (len - ByteString.length bs) filler <> bs
   | otherwise = bs

sumOf :: FileHeader () Identity -> Word32
sumOf header = ByteString.foldl' add 0 (fold $ serialize blankFormat header)
   where add s b = s + fromIntegral b
         blankFormat = record (fileHeaderRecord $ literal $ ASCII.replicate 8 ' ')

instance TextualMonoid ByteString where
   fromText = encodeUtf8
   singleton = ASCII.singleton
   splitCharacterPrefix = ASCII.uncons
   dropWhile _ = ASCII.dropWhile
   dropWhile_ _ = ASCII.dropWhile
   takeWhile _ = ASCII.takeWhile
   takeWhile_ _ = ASCII.takeWhile
   span _ = ASCII.span
   span_ _ = ASCII.span

$(Rank2.TH.deriveAll ''Archive)
$(Rank2.TH.deriveAll ''File)
$(Rank2.TH.deriveAll ''FileHeader)
$(Rank2.TH.deriveAll ''UStarHeader)

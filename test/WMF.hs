{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TemplateHaskell #-}

module WMF where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor.Identity (Identity)
import Data.Int
import Data.Word
import Data.Serialize
import qualified Rank2.TH
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser)

import Construct

import Prelude hiding ((<$), (*>))

data File f = File{
   placeableHeader     :: f (Maybe (PlaceableHeader Identity)),
   wmfType             :: f WMFType,
   version             :: f Word16,
   size                :: f Word32,
   numberOfObjects     :: f Word16,
   sizeOfLargestRecord :: f Word32,
   numberOfParams      :: f Word16,
   records             :: f [Record Identity]
   }

deriving instance Show (File Identity)

data PlaceableHeader f = PlaceableHeader{
   handle       :: f Word16,
   left         :: f Int16,
   top          :: f Int16,
   right        :: f Int16,
   bottom       :: f Int16,
   unitsPerInch :: f Word16,
   checksum     :: f Word16
   }

deriving instance Show (PlaceableHeader Identity)

data Record f = Record{
   recordSize   :: f Word32,
   function     :: f Function,
   params       :: f [Word16]
   }

deriving instance Show (Record Identity)

data WMFType = InMemory | InFile deriving (Eq, Ord, Show)

data Function = AbortDoc
              | Aldus_Header
              | AnimatePalette
              | Arc
              | BitBlt
              | Chord
              | CLP_Header16
              | CLP_Header32
              | CreateBitmap
              | CreateBitmapIndirect
              | CreateBrush
              | CreateBrushIndirect
              | CreateFontIndirect
              | CreatePalette
              | CreatePatternBrush
              | CreatePenIndirect
              | CreateRegion
              | DeleteObject
              | DibBitblt
              | DibCreatePatternBrush
              | DibStretchBlt
              | DrawText
              | Ellipse
              | EndDoc
              | EndPage
              | EOF
              | Escape
              | ExcludeClipRect
              | ExtFloodFill
              | ExtTextOut
              | FillRegion
              | FloodFill
              | FrameRegion
              | Header
              | IntersectClipRect
              | InvertRegion
              | LineTo
              | MoveTo
              | OffsetClipRgn
              | OffsetViewportOrg
              | OffsetWindowOrg
              | PaintRegion
              | PatBlt
              | Pie
              | Polygon
              | Polyline
              | PolyPolygon
              | RealizePalette
              | Rectangle
              | ResetDC
              | ResizePalette
              | RestoreDC
              | RoundRect
              | SaveDC
              | ScaleViewportExt
              | ScaleWindowExt
              | SelectClipRegion
              | SelectObject
              | SelectPalette
              | SetBKColor
              | SetBKMode
              | SetDibToDev
              | SelLayout
              | SetMapMode
              | SetMapperFlags
              | SetPalEntries
              | SetPixel
              | SetPolyFillMode
              | SetReLabs
              | SetROP2
              | SetStretchBltMode
              | SetTextAlign
              | SetTextCharExtra
              | SetTextColor
              | SetTextJustification
              | SetViewportExt
              | SetViewportOrg
              | SetWindowExt
              | SetWindowOrg
              | StartDoc
              | StartPage
              | StretchBlt
              | StretchDIB
              | TextOut
              deriving (Eq, Ord, Show)

fileFormat :: Format (Parser ByteString) Maybe ByteString (File Identity)
fileFormat = record File{
   placeableHeader = optional headerFormat,
   wmfType = InMemory <$ literal (ByteString.pack [0, 0])
             <|> InFile <$ literal (ByteString.pack [1, 0]),
   version = literal (ByteString.pack [9, 0]) *> cereal' getWord16le putWord16le,
   size = cereal' getWord32le putWord32le,
   numberOfObjects = cereal' getWord16le putWord16le,
   sizeOfLargestRecord = cereal' getWord32le putWord32le,
   numberOfParams = cereal' getWord16le putWord16le,
   records = many recordFormat
   }
   
headerFormat :: Format (Parser ByteString) Maybe ByteString (PlaceableHeader Identity)
headerFormat = record PlaceableHeader{
   handle = literal (ByteString.pack [0xD7, 0xCD, 0xC6, 0x9A]) *> cereal' getWord16le putWord16le,
   left = cereal' getInt16le putInt16le,
   top = cereal' getInt16le putInt16le,
   right = cereal' getInt16le putInt16le,
   bottom = cereal' getInt16le putInt16le,
   unitsPerInch = cereal' getWord16le putWord16le,
   checksum = literal (ByteString.pack [0, 0, 0, 0]) *> cereal' getWord16le putWord16le
   }

recordFormat :: Format (Parser ByteString) Maybe ByteString (Record Identity)
recordFormat = mfix $ \this-> record Record{
   recordSize = cereal' getWord32le putWord32le,
   function = AbortDoc <$ literal (ByteString.pack [0x52, 0x00]) <|>
              Aldus_Header <$ literal (ByteString.pack [0x01, 0x00]) <|>
              AnimatePalette <$ literal (ByteString.pack [0x36, 0x04]) <|>
              Arc <$ literal (ByteString.pack [0x17, 0x08]) <|>
              BitBlt <$ literal (ByteString.pack [0x22, 0x09]) <|>
              Chord <$ literal (ByteString.pack [0x30, 0x08]) <|>
              CLP_Header16 <$ literal (ByteString.pack [0x02, 0x00]) <|>
              CLP_Header32 <$ literal (ByteString.pack [0x03, 0x00]) <|>
              CreateBitmap <$ literal (ByteString.pack [0xFE, 0x06]) <|>
              CreateBitmapIndirect <$ literal (ByteString.pack [0xFD, 0x02]) <|>
              CreateBrush <$ literal (ByteString.pack [0xF8, 0x00]) <|>
              CreateBrushIndirect <$ literal (ByteString.pack [0xFC, 0x02]) <|>
              CreateFontIndirect <$ literal (ByteString.pack [0xFB, 0x02]) <|>
              CreatePalette <$ literal (ByteString.pack [0xF7, 0x00]) <|>
              CreatePatternBrush <$ literal (ByteString.pack [0xF9, 0x01]) <|>
              CreatePenIndirect <$ literal (ByteString.pack [0xFA, 0x02]) <|>
              CreateRegion <$ literal (ByteString.pack [0xFF, 0x06]) <|>
              DeleteObject <$ literal (ByteString.pack [0xF0, 0x01]) <|>
              DibBitblt <$ literal (ByteString.pack [0x40, 0x09]) <|>
              DibCreatePatternBrush <$ literal (ByteString.pack [0x42, 0x01]) <|>
              DibStretchBlt <$ literal (ByteString.pack [0x41, 0x0B]) <|>
              DrawText <$ literal (ByteString.pack [0x2F, 0x06]) <|>
              Ellipse <$ literal (ByteString.pack [0x18, 0x04]) <|>
              EndDoc <$ literal (ByteString.pack [0x5E, 0x00]) <|>
              EndPage <$ literal (ByteString.pack [0x50, 0x00]) <|>
              EOF <$ literal (ByteString.pack [0x00, 0x00]) <|>
              Escape <$ literal (ByteString.pack [0x26, 0x06]) <|>
              ExcludeClipRect <$ literal (ByteString.pack [0x15, 0x04]) <|>
              ExtFloodFill <$ literal (ByteString.pack [0x48, 0x05]) <|>
              ExtTextOut <$ literal (ByteString.pack [0x32, 0x0A]) <|>
              FillRegion <$ literal (ByteString.pack [0x28, 0x02]) <|>
              FloodFill <$ literal (ByteString.pack [0x19, 0x04]) <|>
              FrameRegion <$ literal (ByteString.pack [0x29, 0x04]) <|>
              Header <$ literal (ByteString.pack [0x04, 0x00]) <|>
              IntersectClipRect <$ literal (ByteString.pack [0x16, 0x04]) <|>
              InvertRegion <$ literal (ByteString.pack [0x2A, 0x01]) <|>
              LineTo <$ literal (ByteString.pack [0x13, 0x02]) <|>
              MoveTo <$ literal (ByteString.pack [0x14, 0x02]) <|>
              OffsetClipRgn <$ literal (ByteString.pack [0x20, 0x02]) <|>
              OffsetViewportOrg <$ literal (ByteString.pack [0x11, 0x02]) <|>
              OffsetWindowOrg <$ literal (ByteString.pack [0x0F, 0x02]) <|>
              PaintRegion <$ literal (ByteString.pack [0x2B, 0x01]) <|>
              PatBlt <$ literal (ByteString.pack [0x1D, 0x06]) <|>
              Pie <$ literal (ByteString.pack [0x1A, 0x08]) <|>
              Polygon <$ literal (ByteString.pack [0x24, 0x03]) <|>
              Polyline <$ literal (ByteString.pack [0x25, 0x03]) <|>
              PolyPolygon <$ literal (ByteString.pack [0x38, 0x05]) <|>
              RealizePalette <$ literal (ByteString.pack [0x35, 0x00]) <|>
              Rectangle <$ literal (ByteString.pack [0x1B, 0x04]) <|>
              ResetDC <$ literal (ByteString.pack [0x4C, 0x01]) <|>
              ResizePalette <$ literal (ByteString.pack [0x39, 0x01]) <|>
              RestoreDC <$ literal (ByteString.pack [0x27, 0x01]) <|>
              RoundRect <$ literal (ByteString.pack [0x1C, 0x06]) <|>
              SaveDC <$ literal (ByteString.pack [0x1E, 0x00]) <|>
              ScaleViewportExt <$ literal (ByteString.pack [0x12, 0x04]) <|>
              ScaleWindowExt <$ literal (ByteString.pack [0x10, 0x04]) <|>
              SelectClipRegion <$ literal (ByteString.pack [0x2C, 0x01]) <|>
              SelectObject <$ literal (ByteString.pack [0x2D, 0x01]) <|>
              SelectPalette <$ literal (ByteString.pack [0x34, 0x02]) <|>
              SetBKColor <$ literal (ByteString.pack [0x01, 0x02]) <|>
              SetBKMode <$ literal (ByteString.pack [0x02, 0x01]) <|>
              SetDibToDev <$ literal (ByteString.pack [0x33, 0x0D]) <|>
              SelLayout <$ literal (ByteString.pack [0x49, 0x01]) <|>
              SetMapMode <$ literal (ByteString.pack [0x03, 0x01]) <|>
              SetMapperFlags <$ literal (ByteString.pack [0x31, 0x02]) <|>
              SetPalEntries <$ literal (ByteString.pack [0x37, 0x00]) <|>
              SetPixel <$ literal (ByteString.pack [0x1F, 0x04]) <|>
              SetPolyFillMode <$ literal (ByteString.pack [0x06, 0x01]) <|>
              SetReLabs <$ literal (ByteString.pack [0x05, 0x01]) <|>
              SetROP2 <$ literal (ByteString.pack [0x04, 0x01]) <|>
              SetStretchBltMode <$ literal (ByteString.pack [0x07, 0x01]) <|>
              SetTextAlign <$ literal (ByteString.pack [0x2E, 0x01]) <|>
              SetTextCharExtra <$ literal (ByteString.pack [0x08, 0x01]) <|>
              SetTextColor <$ literal (ByteString.pack [0x09, 0x02]) <|>
              SetTextJustification <$ literal (ByteString.pack [0x0A, 0x02]) <|>
              SetViewportExt <$ literal (ByteString.pack [0x0E, 0x02]) <|>
              SetViewportOrg <$ literal (ByteString.pack [0x0D, 0x02]) <|>
              SetWindowExt <$ literal (ByteString.pack [0x0C, 0x02]) <|>
              SetWindowOrg <$ literal (ByteString.pack [0x0B, 0x02]) <|>
              StartDoc <$ literal (ByteString.pack [0x4D, 0x01]) <|>
              StartPage <$ literal (ByteString.pack [0x4F, 0x00]) <|>
              StretchBlt <$ literal (ByteString.pack [0x23, 0x0B]) <|>
              StretchDIB <$ literal (ByteString.pack [0x43, 0x0F]) <|>
              TextOut <$ literal (ByteString.pack [0x21, 0x05]),
   params = count (fromIntegral (recordSize this) - 3) (cereal' getWord16le putWord16le)
   }

$(Rank2.TH.deriveAll ''File)
$(Rank2.TH.deriveAll ''PlaceableHeader)
$(Rank2.TH.deriveAll ''Record)

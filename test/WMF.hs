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
   function = AbortDoc <$ word16le 0x0052 <|>
              Aldus_Header <$ word16le 0x0001 <|>
              AnimatePalette <$ word16le 0x0436 <|>
              Arc <$ word16le 0x0817 <|>
              BitBlt <$ word16le 0x0922 <|>
              Chord <$ word16le 0x0830 <|>
              CLP_Header16 <$ word16le 0x0002 <|>
              CLP_Header32 <$ word16le 0x0003 <|>
              CreateBitmap <$ word16le 0x06FE <|>
              CreateBitmapIndirect <$ word16le 0x02FD <|>
              CreateBrush <$ word16le 0x00F8 <|>
              CreateBrushIndirect <$ word16le 0x02FC <|>
              CreateFontIndirect <$ word16le 0x02FB <|>
              CreatePalette <$ word16le 0x00F7 <|>
              CreatePatternBrush <$ word16le 0x01F9 <|>
              CreatePenIndirect <$ word16le 0x02FA <|>
              CreateRegion <$ word16le 0x06FF <|>
              DeleteObject <$ word16le 0x01F0 <|>
              DibBitblt <$ word16le 0x0940 <|>
              DibCreatePatternBrush <$ word16le 0x0142 <|>
              DibStretchBlt <$ word16le 0x0B41 <|>
              DrawText <$ word16le 0x062F <|>
              Ellipse <$ word16le 0x0418 <|>
              EndDoc <$ word16le 0x005E <|>
              EndPage <$ word16le 0x0050 <|>
              EOF <$ word16le 0x0000 <|>
              Escape <$ word16le 0x0626 <|>
              ExcludeClipRect <$ word16le 0x0415 <|>
              ExtFloodFill <$ word16le 0x0548 <|>
              ExtTextOut <$ word16le 0x0A32 <|>
              FillRegion <$ word16le 0x0228 <|>
              FloodFill <$ word16le 0x0419 <|>
              FrameRegion <$ word16le 0x0429 <|>
              Header <$ word16le 0x0004 <|>
              IntersectClipRect <$ word16le 0x0416 <|>
              InvertRegion <$ word16le 0x012A <|>
              LineTo <$ word16le 0x0213 <|>
              MoveTo <$ word16le 0x0214 <|>
              OffsetClipRgn <$ word16le 0x0220 <|>
              OffsetViewportOrg <$ word16le 0x0211 <|>
              OffsetWindowOrg <$ word16le 0x020F <|>
              PaintRegion <$ word16le 0x012B <|>
              PatBlt <$ word16le 0x061D <|>
              Pie <$ word16le 0x081A <|>
              Polygon <$ word16le 0x0324 <|>
              Polyline <$ word16le 0x0325 <|>
              PolyPolygon <$ word16le 0x0538 <|>
              RealizePalette <$ word16le 0x0035 <|>
              Rectangle <$ word16le 0x041B <|>
              ResetDC <$ word16le 0x014C <|>
              ResizePalette <$ word16le 0x0139 <|>
              RestoreDC <$ word16le 0x0127 <|>
              RoundRect <$ word16le 0x061C <|>
              SaveDC <$ word16le 0x001E <|>
              ScaleViewportExt <$ word16le 0x0412 <|>
              ScaleWindowExt <$ word16le 0x0410 <|>
              SelectClipRegion <$ word16le 0x012C <|>
              SelectObject <$ word16le 0x012D <|>
              SelectPalette <$ word16le 0x0234 <|>
              SetBKColor <$ word16le 0x0201 <|>
              SetBKMode <$ word16le 0x0102 <|>
              SetDibToDev <$ word16le 0x0D33 <|>
              SelLayout <$ word16le 0x0149 <|>
              SetMapMode <$ word16le 0x0103 <|>
              SetMapperFlags <$ word16le 0x0231 <|>
              SetPalEntries <$ word16le 0x0037 <|>
              SetPixel <$ word16le 0x041F <|>
              SetPolyFillMode <$ word16le 0x0106 <|>
              SetReLabs <$ word16le 0x0105 <|>
              SetROP2 <$ word16le 0x0104 <|>
              SetStretchBltMode <$ word16le 0x0107 <|>
              SetTextAlign <$ word16le 0x012E <|>
              SetTextCharExtra <$ word16le 0x0108 <|>
              SetTextColor <$ word16le 0x0209 <|>
              SetTextJustification <$ word16le 0x020A <|>
              SetViewportExt <$ word16le 0x020E <|>
              SetViewportOrg <$ word16le 0x020D <|>
              SetWindowExt <$ word16le 0x020C <|>
              SetWindowOrg <$ word16le 0x020B <|>
              StartDoc <$ word16le 0x014D <|>
              StartPage <$ word16le 0x004F <|>
              StretchBlt <$ word16le 0x0B23 <|>
              StretchDIB <$ word16le 0x0F43 <|>
              TextOut <$ word16le 0x0521,
   params = count (fromIntegral (recordSize this) - 3) (cereal' getWord16le putWord16le)
   }
   where word16le = value (cereal' getWord16le putWord16le)

$(Rank2.TH.deriveAll ''File)
$(Rank2.TH.deriveAll ''PlaceableHeader)
$(Rank2.TH.deriveAll ''Record)

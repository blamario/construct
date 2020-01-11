{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TemplateHaskell #-}
module MBR where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor.Identity (Identity)
import Data.Word
import qualified Rank2.TH
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser)

import Construct
import Construct.Bits

import Prelude hiding ((<$), head)

data MasterBootRecord f = MasterBootRecord{
   bootLoaderCode :: f [Word8],
   partitions     :: f [Partition Identity]
   }

data Partition f = Partition{
   state          :: f PartitionState,
   beginning      :: f (Position Identity),
   partitionType  :: f PartitionType,
   ending         :: f (Position Identity),
   sectorOffset   :: f Word32,
   size           :: f Word32}

data PartitionState = Inactive | Active
                    deriving (Eq, Read, Show)

data PartitionType = NoType
                   | FAT12
                   | XENIX_ROOT
                   | XENIX_USR
                   | FAT16_old
                   | Extended_DOS
                   | FAT16
                   | NTFS
                   | FAT32
                   | FAT32_LBA
                   | ExtendedWithLBA
                   | LINUX_SWAP
                   | LINUX_NATIVE
                   deriving (Eq, Read, Show)

data Position f = Position{
   head           :: f Word8,
   sector         :: f Bits,
   cylinder       :: f Bits}

deriving instance Show (MasterBootRecord Identity)
deriving instance Show (Partition Identity)
deriving instance Show (Position Identity)

format :: Format (Parser ByteString) Maybe ByteString (MasterBootRecord Identity)
format = record MasterBootRecord{
   bootLoaderCode = count 446 byte,
   partitions = count 4 partition}

partition :: Format (Parser ByteString) Maybe ByteString (Partition Identity)
partition = record Partition{
        state = Inactive <$ literal (ByteString.singleton 0) <|>
                Active <$ literal (ByteString.singleton 0x80),
        beginning = position,
        partitionType = NoType <$ literal (ByteString.singleton 0) <|>
                        FAT12 <$ literal (ByteString.singleton 1) <|>
                        XENIX_ROOT <$ literal (ByteString.singleton 2) <|>
                        XENIX_USR <$ literal (ByteString.singleton 3) <|>
                        FAT16_old <$ literal (ByteString.singleton 4) <|>
                        Extended_DOS <$ literal (ByteString.singleton 5) <|>
                        FAT16 <$ literal (ByteString.singleton 6) <|>
                        NTFS <$ literal (ByteString.singleton 7) <|>
                        FAT32 <$ literal (ByteString.singleton 0xB) <|>
                        FAT32_LBA <$ literal (ByteString.singleton 0xC) <|>
                        ExtendedWithLBA <$ literal (ByteString.singleton 0xF) <|>
                        LINUX_SWAP <$ literal (ByteString.singleton 0x82) <|>
                        LINUX_NATIVE <$ literal (ByteString.singleton 0x83),
        ending = position,
        sectorOffset = cereal,
        size = cereal}

position :: Format (Parser ByteString) Maybe ByteString (Position Identity)
position = littleEndianBytesOf $ record Position{
   head = littleEndianBitsOf byte,
   sector = count 6 bit,
   cylinder = count 10 bit}

$(Rank2.TH.deriveAll ''MasterBootRecord)
$(Rank2.TH.deriveAll ''Partition)
$(Rank2.TH.deriveAll ''Position)

module OrphanInstances where

import Data.ByteString (ByteString)
import Data.Monoid.Textual (TextualMonoid)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Monoid.Textual
import qualified Data.ByteString.Char8 as ASCII

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

Construct.hs
============

This is a Haskell implementation of Python's [Construct](https://construct.readthedocs.io/en/latest/intro.html)
library. It provides a succinct and easy way to specify data formats. Before you get to the succinct part, though,
you'll probably need a bunch of extensions and imports:

~~~ {.haskell}
{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TemplateHaskell #-}

module README where

import Data.Functor.Identity (Identity(Identity))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ASCII
import qualified Rank2.TH
import Text.ParserCombinators.Incremental.LeftBiasedLocal (Parser, completeResults, feed, feedEof)
import Construct

import Prelude hiding ((*>), (<*))
~~~

Example
-------

With that out of the way, let's take the simple example format from the original. Here's what its specification looks
like in Haskell:

~~~ {.haskell}
data BitMap f = BitMap{
   width :: f Word8,
   height :: f Word8,
   pixels :: f [[Word8]]
   }
deriving instance Show (BitMap Identity)
$(Rank2.TH.deriveAll ''BitMap)

format :: Format (Parser ByteString) Identity ByteString (BitMap Identity)
format = literal (ASCII.pack "BMP") *> mfix (\this-> record
  BitMap{
        width= byte,
        height= byte,
        pixels= count (fromIntegral $ height this) (count (fromIntegral $ width this) byte)
        })
~~~

There are two parts to the specification.

The `data BitMap` declaration specifies the in-memory layout of a simple bitmap. Note that it's declared as a record
with every field wrapped in a type constructor parameter. This declaration style is sometimes called [Higher-Kinded
Data](https://reasonablypolymorphic.com/blog/higher-kinded-data/). To regain a regular record, just instantiate the
parameter to `Identity` &mdash; a `BitMap Identity` contains exactly one value of each field.

The other part of the specification is the `format` definition that specifies the bi-directional mapping between the
in-memory and the serialized form of the bitmap. An isomorphism, to be precise. The two definitions are enough to automatically
serialize the in-memory record form into the binary form:

~~~ {.haskell}
-- |
-- >>> serialize format BitMap{width= Identity 3, height= Identity 2, pixels=Identity [[7, 8, 9], [11, 12, 13]]}
-- Identity "BMP\ETX\STX\a\b\t\v\f\r"
~~~

and to parse the serialized binary form back into the record structure:

~~~ {.haskell}
-- |
-- >>> completeResults $ feedEof $ feed (ASCII.pack "BMP" <> ByteString.pack [3, 2, 7, 8, 9, 11, 12, 13]) $ parse format
-- [(BitMap {width = Identity 3, height = Identity 2, pixels = Identity [[7,8,9],[11,12,13]]},"")]
~~~

Examples of more complex and realistic formats can be found in the `test/examples` directory.

Acknowledgements
----------------

I owe the inspiration for this library to Yair Chuchem and his
[post](https://yairchu.github.io/posts/codecs-as-prisms.html) that introduced me to Construct. I must also express
gratitude to the authors of the original library of course. And finally, to the authors of the paper [Invertible
Syntax Descriptions:Unifying Parsing and Pretty
Printing](https://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf) which I remembered reading
just in time to avoid following some bad ideas.

Implementation notes
--------------------

I had to overcome two problems while implementing the present library. The first difficulty, mentioned in the
aforementioned blog post, is how to convert a record of formats into a format of the record. As the author of
[rank2classes](https://hackage.haskell.org/package/rank2classes), I went for an obvious solution: parameterize the
record as seen in the example, make it an instance of the
[`Rank2.Traversable`](https://hackage.haskell.org/package/rank2classes-1.3.1.2/docs/Rank2.html#t:Traversable) class,
and apply [`Rank2.traverse`](https://hackage.haskell.org/package/rank2classes-1.3.1.2/docs/Rank2.html#v:traverse) to
it.

That little trick alone would have been enough for a nearly complete package, except the Python library also enables a
record field to refer to any of the preceding fields. This is not a problem when serializing, but when parsing it
means that a parser must be able to refer to an already-parsed part of the value that's still being parsed.

The standard solution to the problem of accessing the results (parsed values) of a computation (parsing) *while within
the computation* is known as the
[`MonadFix`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fix.html#t:MonadFix) class, though
perhaps not as widely as it deserves. You won't find any parsers in the list of its instances, though, and for a good
reason - it's quite impossible for a parser to obtain a value that it hasn't parsed yet. All *we* need, luckily, is
access a partially parsed value. A limited `MonadFix` instance can do that, provided that we also use a specialized
form of `Rank2.traverse` capable of preserving the record structure while it's being parsed.

As it happens, I have an old parser library named
[incremental-parser](https://hackage.haskell.org/package/incremental-parser) on Hackage, and the name seems quite
appropriate for what I'm doing here. I added the necessary functionality there, but another parser combinator library
should be capable of the same feat. It just needs to implement the `mfix` and `fixSequence` combinators and it can be
used with the present library.

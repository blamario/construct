module Construct.Internal where

-- | The central type. The four type parameters are:
--
--   * @m@, type of the parser for the format
--   * @n@, container type for the serialized form of the value, typically 'Identity' unless something
--     'Alternative' is called for.
--   * @s@, type of the serialized value, typically 'ByteString'
--   * @a@, type of the parsed value in memory
--
-- The @parse@ and @serialize@ fields can be used to perform the two sides of the conversion between the in-memory and
-- serialized form of the value.
data Format m n s a = Format {
   parse :: m a,
   serialize :: a -> n s
   }

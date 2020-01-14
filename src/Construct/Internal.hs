module Construct.Internal where

-- | The central type. The four type parameters are:
--
--   * @m@ is the type of the parser for the format
--   * @n@ is the container type for the serialized form of the value, typically 'Identity' unless something
--     'Alternative' is called for.
--   * @s@ is the type of the serialized value, typically 'ByteString'
--   * @a@ is the type of the value in the program
--
-- The @parse@ and @serialize@ fields can be used to perform the two sides of the conversion between the in-memory and
-- serialized form of the value.
data Format m n s a = Format {
   parse :: m a,
   serialize :: a -> n s
   }

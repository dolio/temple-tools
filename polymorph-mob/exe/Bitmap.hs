
module Bitmap where

import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set

import Temple.Objects.Spec

newtype Bitmap = BM BS.ByteString

countSet :: Bitmap -> Int
countSet (BM bs) = BS.foldl' (\n i -> n + popCount i) 0 bs

-- Gets the object flags set in order in the bitmap.
--
-- Note: the bitmap starts with location, which is 1, not 0 in the object
-- fields list.
setFields :: Bitmap -> [ObjectField]
setFields (BM bs) = BS.foldr thread (const []) bs 0
  where
  thread b k !n
    | b == 0 = k (n+8)
    | otherwise = fmap (fielded . (+n)) (filter (testBit b) [0..7]) ++ k (n+8)

  fielded :: Int -> ObjectField
  fielded i = toEnum $ i + 1

fromFields :: Int -> Set ObjectField -> Bitmap
fromFields size flds =
  BM . BS.pack . slice size 0 . fmap toNum $ Set.toList flds
  where
  toNum = subtract 1 . fromEnum

  chunkToByte n = foldl' setBit 0 . fmap (subtract n)

  slice   0 _  _ = []
  slice pad _ [] = replicate pad 0
  slice pad n ms
    | (chunk, rest) <- break (<= n+7) ms
    = chunkToByte n chunk : slice (pad-1) (n+8) rest

-- How many 32-bit blocks does it take to encode a size `n` bitmap
bits2blocks :: Int -> Int
bits2blocks n | (d,m) <- divMod n 32 = d + if m > 0 then 1 else 0

type2blocks :: ObjectType -> Int
type2blocks ty
  = bits2blocks
  . maximum
  . fmap fromEnum
  . filter (hasField ty)
  . takeWhile (< ExtraF minBound) -- no extra fields
  $ [minBound .. maxBound]

getBitmap :: ObjectType -> Get Bitmap
getBitmap ty = BM <$> getByteString (type2blocks ty * 4)

putBitmap :: Bitmap -> Put
putBitmap (BM bs) = putByteString bs

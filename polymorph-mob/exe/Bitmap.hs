
module Bitmap where

import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.Set (Set)
import Data.Set qualified as Set

newtype Bitmap = BM BS.ByteString
  deriving (Eq, Show)

displayBits :: Bitmap -> Builder
displayBits (BM bs) = BS.foldr f mempty bs
  where
  f b r =
    foldMap (\i -> char8 if testBit b i then '1' else '0') [0..7] <> r

countSet :: Bitmap -> Int
countSet (BM bs) = BS.foldl' (\n i -> n + popCount i) 0 bs

countBlocks :: Bitmap -> Int
countBlocks (BM bs) = BS.length bs `div` 4

-- Gets the object flags set in order in the bitmap. The provided value is what
-- is considered to be position 0 in the bitmap.
setFields :: Enum e => e -> Bitmap -> [e]
setFields off (BM bs) = BS.foldr thread (const []) bs 0
  where
  offn = fromEnum off

  thread b k !n
    | b == 0 = k (n+8)
    | otherwise = fmap (fielded . (+n)) (filter (testBit b) [0..7]) ++ k (n+8)

  fielded i = toEnum $ i + offn

-- Creates a bitmap from a set of values. The provided single element is what
-- index 0 of the bitmap will be treated as, so all values in the set must
-- be greater or equal to that value. The size specifies the smallest number
-- of bytes that the bitmap must occupy, but it will be rounded up to a 4-byte
-- block.
fromFields :: Enum e => e -> Int -> Set e -> Bitmap
fromFields off size0 flds =
  BM . BS.pack . slice size 0 . fmap toNum $ Set.toList flds
  where
  size | (d, m) <- divMod size0 4 = 4 * (d + if m == 0 then 0 else 1)
  offn = fromEnum off
  toNum = subtract offn . fromEnum

  chunkToByte n = foldl' setBit 0 . fmap (subtract n)

  slice   0 _  _ = []
  slice pad _ [] = replicate pad 0
  slice pad n ms
    | (chunk, rest) <- break (<= n+7) ms
    = chunkToByte n chunk : slice (pad-1) (n+8) rest

-- How many 32-bit blocks does it take to encode a size `n` bitmap
bits2blocks :: Int -> Int
bits2blocks n | (d,m) <- divMod n 32 = d + if m > 0 then 1 else 0

-- Constructs a dense bitmap with the first `n` bits set.
denseBitmap :: Int -> Bitmap
denseBitmap (flip divMod 32 -> (d,m))
  = BM . BS.toStrict . toLazyByteString
  $ repOn d (word32LE 0xffffffff)
      (if m == 0 then mempty else word32LE (bit m - 1))
  where
  repOn 0  _ acc = acc
  repOn d bu acc
    | even d = repOn (div d 2) (bu <> bu) acc
    | otherwise = repOn (div d 2) (bu <> bu) (bu <> acc)

-- Checks whether a bitmap is dense for `n` values.
isDense :: Int -> Bitmap -> Bool
isDense n (BM bs) = BS.foldr f (== 0) bs n where
  f b k n
    | n <= 8 = b == bit n - 1 && k 0
    | otherwise = b == 0xff && k (n - 8)

-- Gets a bitmap that can store `size` _bits_ of information. All bitmaps are
-- aligned to 32-bit blocks.
getBitmap :: Int -> Get Bitmap
getBitmap size = getBitmapBlocks $ bits2blocks size

-- Gets a bitmap whose size is specified in 32-bit blocks.
getBitmapBlocks :: Int -> Get Bitmap
getBitmapBlocks bsize = BM <$> getByteString (4 * bsize)

putBitmap :: Bitmap -> Put
putBitmap (BM bs) = putByteString bs

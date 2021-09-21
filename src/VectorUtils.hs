module VectorUtils where

import Prelude hiding (index, length)
import Data.Word (Word8)
import Data.Vector (Vector, (//), generate)
import Data.ByteString (ByteString, index, length)

indexPairs :: Vector a -> [(Int,a)]
indexPairs = snd . foldl indexPair (0,[])
  where
    indexPair (i,xs) a = (i+1,(i,a) : xs)

copyTo :: Vector a -> (Int, Vector a) -> Vector a
copyTo vfrom (offset, vto) = vto // offsetPairs vfrom
  where
    offsetBy o (i,a) = (i + o,a)
    offsetPairs = fmap (offsetBy offset) . indexPairs

fromByteString :: ByteString -> Vector Word8
fromByteString bs = generate len byteForIndex
  where
    len = length bs
    byteForIndex = index bs
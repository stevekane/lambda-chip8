module VectorUtils where

import Prelude hiding (index, length)
import Data.Word (Word8)
import Data.Vector (Vector, (//), (!), generate, filter)
import Data.ByteString (ByteString, index, length)

indexPairs :: Vector a -> [(Int,a)]
indexPairs = snd . foldl indexPair (0,[])
  where
    indexPair (i,xs) a = (i+1,(i,a) : xs)

copyTo :: (Vector a, Int) -> (Vector a, Int) -> Int -> Vector a
copyTo (to,t0) (from,f0) n = to // fmap updatePair [0..n]
  where updatePair i = (t0 + i,from ! (f0 + i))

fromByteString :: ByteString -> Vector Word8
fromByteString bs = generate len byteForIndex
  where
    len = length bs
    byteForIndex = index bs
module Lib where

import qualified Data.ByteString as BS
import Data.Array
import Data.Word
import Data.Bits 

toWord8 :: Bool -> Word8
toWord8 True = 0x1
toWord8 False = 0x0

saturateWord8 :: Bool -> Word8
saturateWord8 True = 0xFF
saturateWord8 False = 0x00

bcd :: Word8 -> (Word8, Word8, Word8)
bcd b = (hundreds,tens,ones)
  where
    ones = b `mod` 10
    tens = b `div` 10 `mod` 10
    hundreds = b `div` 100 `mod` 10

highNibble :: Word8 -> Word8 -- no available Word4 type
highNibble b = (b .&. 0xF0) `shiftR` 4

lowNibble :: Word8 -> Word8 -- no available Word4 type
lowNibble b = b .&. 0x0F

nthbit :: Integral i => i -> Word8 -> Bool
nthbit n b = (b .&. (1 `shiftL` fromIntegral n)) > 0

word8 :: Integral a => a -> Word8
word8 = fromIntegral

word16 :: Integral a => a -> Word16
word16 = fromIntegral

word32 :: Integral a => a -> Word32
word32 = fromIntegral

int :: Integral a => a -> Int
int = fromIntegral

word8FromNibbles :: Word8 -> Word8 -> Word8
word8FromNibbles h l = high4 + low4
  where
    high4 = word8 h `shiftL` 4
    low4 = l

word16FromNibbles :: Word8 -> Word8 -> Word8 -> Word16
word16FromNibbles h m l = high4 + mid4 + low4
  where
    high4 = word16 h `shiftL` 8
    mid4 = word16 m `shiftL` 4
    low4 = word16 l

add :: Word8 -> Word8 -> (Word8, Bool)
add a b = (a + b, (word16 a + word16 b) > 255)

sub :: Word8 -> Word8 -> (Word8, Bool)
sub a b = (a - b,a < b)

initialize :: (Integral i, Ix i) => (i,i) -> e -> Array i e
initialize (min,max) v0 = array (min,max) (fmap initialPair [min..max])
  where initialPair i = (i,v0)

(×) :: Integral a => [a] -> [a] -> [(a,a)]
l × r = [ (x,y) | y <- r, x <- l ]

toArray :: 
  (Ix i, Num i) => 
  BS.ByteString -> 
  Array i Word8
toArray bs = array (0,maxIndex) (fmap toAssoc [0..(length-1)])
  where 
    length = BS.length bs
    maxIndex = fromIntegral (length - 1)
    toAssoc i = (fromIntegral i, BS.index bs i)

copyTo :: 
  (Ix a, Ix b, Integral a, Integral b) => 
  (Array a e, a) -> 
  (Array b e, b) -> 
  a -> 
  Array a e
copyTo (to,t0) (from,f0) n = to // fmap updatePair [0..n]
  where updatePair i = (t0 + fromIntegral i,from ! (f0 + fromIntegral i))

shiftBy :: Integral i => i -> (i,i) -> (i,i)
shiftBy o (i,j) = (i + o,j)

wrap :: 
  (Integral a, Integral b) => 
  (a,b) -> 
  (a,b) -> 
  (a,b)
wrap (w,h) (x,y) = (mod x w,mod y h)



bound :: 
  (Integral a, Integral b) => 
  (a,b) -> 
  (a,b) -> 
  (a,b) -> 
  (a,b)
bound (xMax,yMax) (w,h) (x,y) = (xBound,yBound) 
  where
    xBound = min (x + w) xMax - 1
    yBound = min (y + h) yMax - 1

showColumns :: 
  (Ix i, Integral i, Show a) => 
  i -> 
  i -> 
  Array i a -> 
  String
showColumns c p a = foldr render "" (assocs a)
  where 
    prefix i = if i `mod` c == 0 then "\n" else ""
    spacer i = if i `mod` p == 0 then "\t\t" else "\t"
    render (i,y) x = prefix i ++ show y ++ spacer (i + 1) ++ x

ntimes :: Int -> (a -> a) -> a -> a
ntimes 0 f x = x
ntimes n f x = ntimes (n - 1) f (f x)
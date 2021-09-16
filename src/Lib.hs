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

digits :: Word8 -> (Word8, Word8, Word8)
digits b = (hundreds, tens, ones)
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

word8FromNibbles :: Word8 -> Word8 -> Word8
word8FromNibbles h l = (h `shiftL` 4) + l

word16FromNibbles :: Word8 -> Word8 -> Word8 -> Word16
word16FromNibbles h m l = (word16 h `shiftL` 8) + (word16 m `shiftL` 4) + word16 l

addWithCarry :: Word8 -> Word8 -> (Word8, Bool)
addWithCarry a b = (a + b, (word16 a + word16 b) > 255)

subtractWithBorrow :: Word8 -> Word8 -> (Word8, Bool)
subtractWithBorrow a b = (a - b, a < b)

(×) :: Integral a => [a] -> [a] -> [(a,a)]
l × r = [ (x,y) | y <- r, x <- l ]

to1DIndex :: Integral a => a -> (a,a) -> a
to1DIndex w (x,y) = y * w + x

initialize :: (Integral i, Ix i) => (i,i) -> e -> Array i e
initialize (min,max) v0 = array (min,max) (fmap initialPair [min..max])
  where initialPair i = (i,v0)

copyTo :: (Ix a, Ix b, Integral a, Integral b) => (Array a e, a) -> (Array b e, b) -> a -> Array a e
copyTo (to,t0) (from,f0) n = to // fmap updatePair [0..n]
  where updatePair i = (t0 + fromIntegral i,from ! (f0 + fromIntegral i))

toArray :: (Ix i, Num i) => BS.ByteString -> Array i Word8
toArray bs = array (0, fromIntegral (length - 1)) (fmap toAssoc [0..(length-1)])
  where 
    length = BS.length bs
    toAssoc i = (fromIntegral i, BS.index bs i)

wrap :: (Integral a, Integral b) => (a,b) -> (a,b) -> (a,b)
wrap (w,h) (x,y) = (mod x w, mod y h)

bound :: (Integral a, Integral b) => (a,b) -> (a,b) -> (a,b) -> (a,b)
bound (xMax,yMax) (w,h) (x,y) = (xBound,yBound) 
  where
    xBound = min (x + w) xMax - 1
    yBound = min (y + h) yMax - 1

showColumns :: (Ix i, Integral i, Show a) => i -> i -> Array i a -> String
showColumns c p a = foldr render "" (assocs a)
  where 
    prefix i = if i `mod` c == 0 then "\n" else ""
    spacer i = if i `mod` p == 0 then "\t\t" else "\t"
    render (i,y) x = prefix i ++ show y ++ spacer (i + 1) ++ x
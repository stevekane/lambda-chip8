module Lib where

import Prelude hiding (length)
import Data.Word
import Data.Bits 
import Data.Vector hiding (length)
import Data.ByteString (ByteString, index, length)


-- | convert boolean to byte values 0 and 1
toWord8 :: Bool -> Word8
toWord8 True = 0x1
toWord8 False = 0x0

-- | convert boolean to byte values 255 and 0
saturateWord8 :: Bool -> Word8
saturateWord8 True = 0xFF
saturateWord8 False = 0x00

-- | binary coded decimal representation of a byte
bcd :: Word8 -> (Word8, Word8, Word8)
bcd b = (hundreds,tens,ones)
  where
    ones = b `mod` 10
    tens = b `div` 10 `mod` 10
    hundreds = b `div` 100 `mod` 10

-- | left-most four bits from a byte
highNibble :: Word8 -> Word8 -- no available Word4 type
highNibble b = (b .&. 0xF0) `shiftR` 4

-- | right-most four bits from a byte
lowNibble :: Word8 -> Word8 -- no available Word4 type
lowNibble b = b .&. 0x0F

-- | value of the nth bit from a byte
nthbit :: Integral i => i -> Word8 -> Bool
nthbit n b = (b .&. (1 `shiftL` fromIntegral n)) > 0

-- | cast to word8
word8 :: Integral a => a -> Word8
word8 = fromIntegral

-- | cast to word16
word16 :: Integral a => a -> Word16
word16 = fromIntegral

-- | cast to word32
word32 :: Integral a => a -> Word32
word32 = fromIntegral

-- | cast to int
int :: Integral a => a -> Int
int = fromIntegral

-- | construct word8 by concatenating two word4s
word8FromNibbles :: Word8 -> Word8 -> Word8
word8FromNibbles h l = high4 + low4
  where
    high4 = word8 h `shiftL` 4
    low4  = l

-- | construct word16 by concatenating three word4s
word16FromNibbles :: Word8 -> Word8 -> Word8 -> Word16
word16FromNibbles h m l = high4 + mid4 + low4
  where
    high4 = word16 h `shiftL` 8
    mid4  = word16 m `shiftL` 4
    low4  = word16 l

-- | bitwise addition with carry bit
add :: Word8 -> Word8 -> (Word8, Bool)
add a b = (a + b, (word16 a + word16 b) > 255)

-- | bitwise subtraction with borrow bit
sub :: Word8 -> Word8 -> (Word8, Bool)
sub a b = (a - b,a < b)

-- | left shift a byte returning the new byte and shifted out bit
lshift :: Word8 -> (Word8, Bool)
lshift w = (w `shiftL` 1,nthbit 7 w)

-- | right shift a byte returning the new byte and shifted out bit
rshift :: Word8 -> (Word8, Bool)
rshift w = (w `shiftR` 1,nthbit 0 w)

-- | cartesian product of two lists
(×) :: Integral a => [a] -> [a] -> [(a,a)]
l × r = [ (x,y) | y <- r, x <- l ]

-- | Repeatedly run a function
ntimes :: Int -> (a -> a) -> a -> a
ntimes 0 f x = x
ntimes n f x = ntimes (n - 1) f (f x)

-- | copy slice from some vector to another vector at an offset
copyTo :: (Vector a, Int) -> (Vector a, Int) -> Int -> Vector a
copyTo (to,t0) (from,f0) n = to // fmap updatePair [0..n]
  where updatePair i = (t0 + i,from ! (f0 + i))

-- | convert bytestring to vector of bytes
fromByteString :: ByteString -> Vector Word8
fromByteString bs = generate len byteForIndex
  where
    len          = length bs
    byteForIndex = index bs
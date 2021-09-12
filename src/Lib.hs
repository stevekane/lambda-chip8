module Lib where

import Data.Word (Word8, Word16)
import Data.Bits 

toWord8 :: Bool -> Word8
toWord8 True = 0x1
toWord8 False = 0x0

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

nthbit :: Int -> Word8 -> Bool
nthbit n b = (b .&. (1 `shiftL` n)) > 0

word8 :: Integral a => a -> Word8
word8 = fromIntegral

word16 :: Integral a => a -> Word16
word16 = fromIntegral

word8FromNibbles :: Word8 -> Word8 -> Word8
word8FromNibbles h l = (h `shiftL` 4) + l

word16FromNibbles :: Word8 -> Word8 -> Word8 -> Word16
word16FromNibbles h m l = (word16 h `shiftL` 8) + (word16 m `shiftL` 4) + word16 l

addWithCarry :: Word8 -> Word8 -> (Word8, Bool)
addWithCarry a b = (a + b, (word16 a + word16 b) > 255)

subtractWithBorrow :: Word8 -> Word8 -> (Word8, Bool)
subtractWithBorrow a b = (a - b, b < a)
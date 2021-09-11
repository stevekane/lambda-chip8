module Lib where

import Data.Word (Word8, Word16)
import Data.Bits 

highNibble b = (b .&. 0xF0) `shiftR` 4
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
word16FromNibbles h m l = fromIntegral h `shiftL` 8 + fromIntegral m `shiftL` 4 + fromIntegral l
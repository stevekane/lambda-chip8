module Chip8 where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector, (!), (//), replicate)
import Prelude hiding (replicate)
import Control.Lens (Lens', set, over, view, (^.))
import Lib ((×), nthbit, highNibble, lowNibble, word8FromNibbles, word16FromNibbles)

type Stack = []

push        = (:)
pop (a : s) = (a,s)
pop []      = error "Pop empty not supported"

class Chip8 c where
  i         :: Lens' c Word16
  pc        :: Lens' c Word16
  stack     :: Lens' c (Stack Word16)
  registers :: Lens' c (Vector Word8)
  ram       :: Lens' c (Vector Word8)
  display   :: Lens' c (Vector Bool)

  execute :: c -> c  
  execute c = ops c
    where 
      i0  = view ram c ! fromIntegral (view pc c)
      i1  = view ram c ! (fromIntegral (view pc c) + 1)
      a   = highNibble i0
      x   = lowNibble i0
      y   = highNibble i1
      z   = lowNibble i1
      vx  = view registers c ! fromIntegral x
      vy  = view registers c ! fromIntegral y
      nn  = word8FromNibbles y z
      nnn = word16FromNibbles x y z
      stepPC c = over pc (+2) c
      skipPC c = over pc (+4) c
      clearDisplay c = set display blankDisplay c
        where
          pixelCount   = length (view display c)
          blankDisplay = replicate pixelCount False
      storePCToStack c = over stack (push pc') c
        where
          pc' = view pc c + 2
      setPCFromStack c = set stack stack' . set pc pc' $ c
        where
          (pc',stack') = pop (view stack c)
      setRegister i v c = set registers registers' c
        where
          registers' = view registers c // [(fromIntegral i, v)]
      ops = case (a,x,y,z) of
        (0x0, 0x0, 0xE, 0x0) -> stepPC . clearDisplay
        (0x0, 0x0, 0xE, 0xE) -> setPCFromStack
        (0x1,   _,   _,   _) -> set pc nnn
        (0x2,   _,   _,   _) -> set pc nnn . storePCToStack
        (0x6,   _,   _,   _) -> stepPC . setRegister x nn
        (0x7,   _,   _,   _) -> stepPC . setRegister x (nn + vy)
        (0xA,   _,   _,   _) -> stepPC . set i nnn
        (0xD,   _,   _,   _) -> stepPC . over display setDisplay
          where
            (x0,y0)          = (fromIntegral vx,fromIntegral vy)
            (w,h)            = (8,fromIntegral z)
            offsets          = [0..(w-1)] × [0..(h-1)]
            ramOffset        = fromIntegral (view i c)
            displayPixels    = view display c
            memory           = view ram c
            setDisplay d     = d // fmap writePixel offsets
            writePixel (i,j) = (index,pixel)
              where
                (x,y)        = (x0 + i, y0 + j)
                index        = fromIntegral (y * 64 + x)
                displayPixel = displayPixels ! index
                spritePixel  = nthbit (7 - i) (memory ! (ramOffset + j))
                pixel        = displayPixel `xor` spritePixel
        _                    -> error (show (a,x,y,z))
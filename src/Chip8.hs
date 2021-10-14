module Chip8 where

import Prelude hiding (replicate)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector, (!), (//), replicate)
import Control.Lens (Lens', set, over, view, (^.))
import Stack (Stack, push, pop)
import VectorUtils (copyTo)
import Lib hiding (copyTo)

class Chip8 c where
  i         :: Lens' c Word16
  pc        :: Lens' c Word16
  s         :: Lens' c Word8
  d         :: Lens' c Word8
  stack     :: Lens' c (Stack Word16)
  registers :: Lens' c (Vector Word8)
  ram       :: Lens' c (Vector Word8)
  display   :: Lens' c (Vector Bool)
  inputs    :: Lens' c (Vector Bool)

  load :: Vector Word8 -> Vector Word8 -> c -> c
  load font prog c = set ram withProg c
    where
      empty    = view ram c
      withFont = font `copyTo` (0,empty)
      withProg = prog `copyTo` (512,withFont)

  execute :: c -> c  
  execute c = ops c
    where 
      i0  = view ram c ! int (view pc c)
      i1  = view ram c ! (int (view pc c) + 1)
      a   = highNibble i0
      x   = lowNibble i0
      y   = highNibble i1
      z   = lowNibble i1
      v0  = view registers c ! 0
      vx  = view registers c ! int x
      vy  = view registers c ! int y
      nn  = word8FromNibbles y z
      nnn = word16FromNibbles x y z
      stepPC = over pc (+2)
      skipPC = over pc (+4)
      clearDisplay = set display (replicate (64*32) False)
      storePCToStack c = over stack (push pc') c
        where
          pc' = view pc c + 2
      setPCFromStack c = set stack stack' . set pc pc' $ c
        where
          (pc',stack') = pop (view stack c)
      setAll t vs = over t (// fmap castIndex vs)
        where
          castIndex (i,v) = (int i,v)
      storeSumAndCarry (x,f) (s,c) = over registers (// [(int x,s), (f,toWord8 c)])
      storeDifferenceAndBorrow (x,f) (d,b) = setAll registers [(x,d), (f,toWord8 (not b))]
      storeBCDAt (hundreds,tens,ones) i = setAll ram [(i,hundreds), (i+1,tens), (i+2,ones)]
      renderSprite x0 y0 height c = over display setDisplay c
        where
          ramOffset        = int (view i c)
          displayPixels    = view display c
          memory           = view ram c
          width            = 8
          offsets          = [0..(width-1)] × [0..(height-1)]
          setDisplay d     = d // fmap writePixel offsets
          writePixel (i,j) = (index,pixel)
            where
              x            = x0 + i
              y            = y0 + j
              index        = int (y*64+x)
              displayPixel = displayPixels ! index
              spritePixel  = nthbit (7-i) (memory ! (ramOffset+j))
              pixel        = displayPixel `xor` spritePixel
      ops = case (a,x,y,z) of
        (0x2,   _,   _,   _) -> set pc nnn . storePCToStack
        (0x0, 0x0, 0xE, 0xE) -> setPCFromStack
        (0x1,   _,   _,   _) -> set pc nnn
        (0xB,   _,   _,   _) -> set pc (nnn + word16 v0)
        (0x3,   _,   _,   _) -> if vx == nn then skipPC else stepPC
        (0x4,   _,   _,   _) -> if vx == nn then stepPC else skipPC
        (0x5,   _,   _,   _) -> if vx == vy then skipPC else stepPC
        (0x9,   _,   _,   _) -> if vx == vy then stepPC else skipPC
        (0x6,   _,   _,   _) -> stepPC . setAll registers [(x,nn)]
        (0x7,   _,   _,   _) -> stepPC . setAll registers [(x,vx+nn)]
        (0x8,   _,   _, 0x0) -> stepPC . setAll registers [(x,vy)]
        (0x8,   _,   _, 0x1) -> stepPC . setAll registers [(x,vx .|. vy)]
        (0x8,   _,   _, 0x2) -> stepPC . setAll registers [(x,vx .&. vy)]
        (0x8,   _,   _, 0x3) -> stepPC . setAll registers [(x,vx `xor` vy)]
        (0x8,   _,   _, 0x4) -> stepPC . storeSumAndCarry (x,0xF) (vx `add` vy)
        (0x8,   _,   _, 0x5) -> stepPC . storeDifferenceAndBorrow (x,0xF) (vx `sub` vy)
        (0x8,   _,   _, 0x7) -> stepPC . storeDifferenceAndBorrow (x,0xF) (vy `sub` vx)
        (0x8,   _,   _, 0x6) -> stepPC . setAll registers [(int x,vx `shiftR` 1), (0xF,toWord8 (nthbit 0 vx))]
        (0x8,   _,   _, 0xE) -> stepPC . setAll registers [(int x,vx `shiftL` 1), (0xF,toWord8 (nthbit 7 vx))]
        -- TODO: Need the random number generator and the one instruction for it!
        -- -- step pc. v(x) = Rand & nn
        -- (0xC, _, _, _) -> stepPC . setV v' . setRandomSeed randomSeed' $ c8
        --   where
        --     (randomWord8,randomSeed') = genWord8 . randomSeed $ c8 
        --     v' = v c8 // [(x,randomWord8 .&. nn)]
        (0x0, 0x0, 0xE, 0x0) -> stepPC . clearDisplay
        (0xD,   _,   _,   _) -> stepPC . renderSprite (int vx) (int vy) (int z)
        (0xA,   _,   _,   _) -> stepPC . set i nnn
        (0xF,   _, 0x1, 0xE) -> stepPC . over i (+ word16 vx)
        (0xF,   _, 0x2, 0x9) -> stepPC . set i (word16 vx * 5)
        (0xF,   _, 0x3, 0x3) -> stepPC . storeBCDAt (bcd vx) (view i c)
        -- TODO: Need to implement a nice copyTo for Vectors to support dump/load
        -- (0xF,   _, 0x5, 0x5) -> stepPC . set ram (copyTo (view ram c,view i c) (view registers c,0) x)
        -- (0xF,   _, 0x6, 0x5) -> stepPC . set ram (copyTo (view registers c,0) (view ram c,view i c) x)
        (0xF,   _, 0x0, 0x7) -> stepPC . setAll registers [(x,view d c)]
        (0xF,   _, 0x1, 0x5) -> stepPC . set d vx 
        (0xF,   _, 0x1, 0x8) -> stepPC . set s vx 
        (0xE,   _, 0x9, 0xE) -> if view inputs c ! int vx then skipPC else stepPC
        (0xE,   _, 0xA, 0x1) -> if view inputs c ! int vx then stepPC else skipPC
        (0xF,   _, 0x0, 0xA) -> if view inputs c ! int vx then skipPC else id
        -- _                    -> error (show (a,x,y,z))
        _                    -> id
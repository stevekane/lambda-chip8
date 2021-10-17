{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Primary Source: https://en.wikipedia.org/wiki/CHIP-8 

module Chip8 (Chip8(..)) where

import Prelude hiding (replicate)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector, (!), (//), replicate)
import Data.Bifunctor (first)
import Control.Lens (Lens', ASetter, set, over, view)
import System.Random (StdGen, genWord8)
import Stack (Stack, push, pop)
import Lib


-- | Constants at top of file to imply credibility
wordsPerInstruction :: Word16 = 2
displayWidth :: Int           = 64
displayHeight :: Int          = 32
fontAddress :: Int            = 0
fontHeight :: Int             = 5
programAddress :: Int         = 512
blankDisplay :: Vector Bool   = replicate (displayWidth*displayHeight) False

-- | Four word8s representing word4s
type Nibbles = (Word8, Word8, Word8, Word8)

-- | Instructions with decoded parameters
data Instruction
  = I_00E0
  | I_00EE
  | I_1NNN { nnn :: Word16 }
  | I_2NNN { nnn :: Word16 }
  | I_3XNN { x :: Word8, nn :: Word8 }
  | I_4XNN { x :: Word8, nn :: Word8 }
  | I_5XY0 { x :: Word8, y :: Word8 }
  | I_6XNN { x :: Word8, nn :: Word8 }
  | I_7XNN { x :: Word8, nn :: Word8 }
  | I_8XY0 { x :: Word8, y :: Word8 }
  | I_8XY1 { x :: Word8, y :: Word8 }
  | I_8XY2 { x :: Word8, y :: Word8 }
  | I_8XY3 { x :: Word8, y :: Word8 }
  | I_8XY4 { x :: Word8, y :: Word8 }
  | I_8XY5 { x :: Word8, y :: Word8 }
  | I_8XY6 { x :: Word8, y :: Word8 }
  | I_8XY7 { x :: Word8, y :: Word8 }
  | I_8XYE { x :: Word8, y :: Word8 }
  | I_9XY0 { x :: Word8, y :: Word8 }
  | I_ANNN { nnn :: Word16 }
  | I_BNNN { nnn :: Word16 }
  | I_CXNN { x :: Word8, nn :: Word8 }
  | I_DXYN { x :: Word8, y :: Word8, n :: Word8 }
  | I_EX9E { x :: Word8 }
  | I_EXA1 { x :: Word8 }
  | I_FX07 { x :: Word8 }
  | I_FX0A { x :: Word8 }
  | I_FX15 { x :: Word8 }
  | I_FX18 { x :: Word8 }
  | I_FX1E { x :: Word8 }
  | I_FX29 { x :: Word8 }
  | I_FX33 { x :: Word8 }
  | I_FX55 { x :: Word8 }
  | I_FX65 { x :: Word8 }
  | I_NOOP { msg :: String }

-- | Chip8 Specification
class Chip8 c where
  rand      :: Lens' c StdGen
  i         :: Lens' c Word16
  pc        :: Lens' c Word16
  s         :: Lens' c Word8
  d         :: Lens' c Word8
  stack     :: Lens' c (Stack Word16)
  registers :: Lens' c (Vector Word8)
  ram       :: Lens' c (Vector Word8)
  display   :: Lens' c (Vector Bool)
  inputs    :: Lens' c (Vector Bool)

  -- | load a font, program, and random seed
  load :: Vector Word8 -> Vector Word8 -> StdGen -> c -> c
  load font prog stdgen c = set ram withProg . set rand stdgen $ c
    where
      empty    = view ram c
      withFont = copyTo (empty,fontAddress) (font,0) (length font)
      withProg = copyTo (withFont,programAddress) (prog,0) (length prog)

  -- | fetch instruction at pc, decode it, and run it
  execute :: c -> c  
  execute c = run instruction c
    where
      nibbles     = fetch c
      instruction = decode nibbles

-- | fetch two bytes from memory at pc and pc+1 and return as word4s
fetch :: Chip8 c => c -> Nibbles
fetch c = (highNibble i0, lowNibble i0, highNibble i1, lowNibble i1)
  where
    i0 = view ram c ! int (view pc c)
    i1 = view ram c ! (int (view pc c) + 1)

-- | decode nibbles into instructions with extracted parameters
decode :: Nibbles -> Instruction
decode (0x0, 0x0, 0xE, 0x0) = I_00E0
decode (0x0, 0x0, 0xE, 0xE) = I_00EE
decode (0x1,  n0,  n1,  n2) = I_1NNN { nnn = word16FromNibbles n0 n1 n2 }
decode (0x2,  n0,  n1,  n2) = I_2NNN { nnn = word16FromNibbles n0 n1 n2 }
decode (0x3,   x,  n0,  n1) = I_3XNN { x = x, nn = word8FromNibbles n0 n1 }
decode (0x4,   x,  n0,  n1) = I_4XNN { x = x, nn = word8FromNibbles n0 n1 }
decode (0x5,   x,   y, 0x0) = I_5XY0 { x = x, y = y }
decode (0x6,   x,  n0,  n1) = I_6XNN { x = x, nn = word8FromNibbles n0 n1 }
decode (0x7,   x,  n0,  n1) = I_7XNN { x = x, nn = word8FromNibbles n0 n1 }
decode (0x8,   x,   y, 0x0) = I_8XY0 { x = x, y = y }
decode (0x8,   x,   y, 0x1) = I_8XY1 { x = x, y = y }
decode (0x8,   x,   y, 0x2) = I_8XY2 { x = x, y = y }
decode (0x8,   x,   y, 0x3) = I_8XY3 { x = x, y = y }
decode (0x8,   x,   y, 0x4) = I_8XY4 { x = x, y = y }
decode (0x8,   x,   y, 0x5) = I_8XY5 { x = x, y = y }
decode (0x8,   x,   y, 0x6) = I_8XY6 { x = x, y = y }
decode (0x8,   x,   y, 0x7) = I_8XY7 { x = x, y = y }
decode (0x8,   x,   y, 0xE) = I_8XYE { x = x, y = y }
decode (0x9,   x,   y, 0x0) = I_9XY0 { x = x, y = y }
decode (0xA,  n0,  n1,  n2) = I_ANNN { nnn = word16FromNibbles n0 n1 n2 }
decode (0xB,  n0,  n1,  n2) = I_BNNN { nnn = word16FromNibbles n0 n1 n2 }
decode (0xC,   x,  n0,  n1) = I_CXNN { x = x, nn = word8FromNibbles n0 n1 }
decode (0xD,   x,   y,   n) = I_DXYN { x = x, y = y, n = n }
decode (0xE,   x, 0x9, 0xE) = I_EX9E { x = x }
decode (0xE,   x, 0xA, 0x1) = I_EXA1 { x = x }
decode (0xF,   x, 0x0, 0x7) = I_FX07 { x = x }
decode (0xF,   x, 0x0, 0xA) = I_FX0A { x = x }
decode (0xF,   x, 0x1, 0x5) = I_FX15 { x = x }
decode (0xF,   x, 0x1, 0x8) = I_FX18 { x = x }
decode (0xF,   x, 0x1, 0xE) = I_FX1E { x = x }
decode (0xF,   x, 0x2, 0x9) = I_FX29 { x = x }
decode (0xF,   x, 0x3, 0x3) = I_FX33 { x = x }
decode (0xF,   x, 0x5, 0x5) = I_FX55 { x = x }
decode (0xF,   x, 0x6, 0x5) = I_FX65 { x = x }
decode nibbles              = I_NOOP { msg = show nibbles }

-- | run an instruction yielding a new Chip8
run :: Chip8 c => Instruction -> c -> c
run instruction c = action c
  where
    action = case instruction of
      I_1NNN { nnn }     -> set pc nnn
      I_2NNN { nnn }     -> set pc nnn . over stack (push (view pc c + 2))
      I_00EE             -> returnFromSubroutine (pop (view stack c))
      I_3XNN { x, nn }   -> if v x c == nn then skipPC else stepPC
      I_4XNN { x, nn }   -> if v x c /= nn then skipPC else stepPC
      I_5XY0 { x, y }    -> if v x c == v y c then skipPC else stepPC
      I_9XY0 { x, y }    -> if v x c /= v y c then skipPC else stepPC
      I_6XNN { x, nn }   -> stepPC . setV x nn
      I_7XNN { x, nn }   -> stepPC . setV x (v x c + nn)
      I_8XY0 { x, y }    -> stepPC . setV x (v y c)
      I_8XY1 { x, y }    -> stepPC . setV x (v x c .|. v y c)
      I_8XY2 { x, y }    -> stepPC . setV x (v x c .&. v y c)
      I_8XY3 { x, y }    -> stepPC . setV x (v x c `xor` v y c)
      I_8XY4 { x, y }    -> stepPC . addWithCarry x (v x c `add` v y c)
      I_8XY5 { x, y }    -> stepPC . subWithBorrow x (v x c `sub` v y c)
      I_8XY7 { x, y }    -> stepPC . subWithBorrow x (v y c `sub` v x c)
      I_8XY6 { x, y }    -> stepPC . shiftWithShifted x (rshift (v x c))
      I_8XYE { x, y }    -> stepPC . shiftWithShifted x (lshift (v x c))
      I_CXNN { x, nn }   -> stepPC . randWithGenerator x nn (genWord8 (view rand c))
      I_00E0             -> stepPC . set display blankDisplay
      I_DXYN { x, y, n } -> stepPC . renderSprite (v x c) (v y c) (int n)
      I_ANNN { nnn }     -> stepPC . set i nnn
      I_BNNN { nnn }     -> stepPC . set pc (nnn + v 0x0 c)
      I_FX1E { x }       -> stepPC . over i (+ word16 (v x c))
      I_FX29 { x }       -> stepPC . set i (word16 (v x c * fontHeight))
      I_FX33 { x }       -> stepPC . storeBCDAt (bcd (v x c)) (view i c)
      I_FX55 { x }       -> stepPC . dumpRegistersToMemory x
      I_FX65 { x }       -> stepPC . loadRegistersFromMemory x
      I_FX07 { x }       -> stepPC . setV x (view d c)
      I_FX15 { x }       -> stepPC . set d (v x c)
      I_FX18 { x }       -> stepPC . set s (v x c)
      I_EX9E { x }       -> if view inputs c ! int (v x c) then skipPC else stepPC
      I_EXA1 { x }       -> if view inputs c ! int (v x c) then stepPC else skipPC
      I_FX0A { x }       -> if view inputs c ! int (v x c) then skipPC else id
      I_NOOP { msg }     -> id

-- | increment the program counter by two bytes
stepPC :: Chip8 c => c -> c
stepPC = over pc (+wordsPerInstruction)

-- | increment the program counter by four bytes
skipPC :: Chip8 c => c -> c
skipPC = over pc (+wordsPerInstruction*2)

-- | fetch the value from a register by index
v :: (Chip8 c, Integral i, Integral o) => i -> c -> o
v i c = fromIntegral (view registers c ! int i)

-- | set the valud in a register by index
setV :: (Chip8 c, Integral i) => i ->  Word8 -> c -> c
setV x value = setAll registers [(x,value)]

-- | set multiple values in a vector-valued lens
setAll :: Integral i => ASetter s t (Vector a) (Vector a) -> [(i, a)] -> s -> t
setAll t vs = over t (// fmap (first int) vs)

-- | return from subroutine by setting the program counter to the popped value
returnFromSubroutine :: Chip8 c => (Word16,Stack Word16) -> c -> c
returnFromSubroutine (pc', stack') = set pc pc' . set stack stack'

-- | store result of bitwise addition in v(x) and carry in v(F)
addWithCarry :: (Chip8 c, Integral i) => i -> (Word8,Bool) -> c -> c
addWithCarry x (s,c) = setV x s . setV 0xF (toWord8 c)

-- | store result of bitwise subtraction in v(x) and !borrow in v(F)
subWithBorrow :: (Chip8 c, Integral i) => i -> (Word8,Bool) -> c -> c
subWithBorrow x (d,b) = setV x d . setV 0xF (toWord8 (not b))

-- | store result of bit-shifting in v(x). store shifted bit in v(F)
shiftWithShifted :: (Chip8 c, Integral i) => i -> (Word8,Bool) -> c -> c
shiftWithShifted x (r,b) = setV x r . setV 0xF (toWord8 b)

-- | store result of generating random value in v(x). store new seed in rand
randWithGenerator:: (Chip8 c, Integral i) => i -> Word8 -> (Word8,StdGen) -> c -> c
randWithGenerator x nn (w,r) = setV x (w .&. nn) . set rand r

-- | store binary-coded decimal representation of a byte in v(i..i+2)
storeBCDAt :: (Chip8 c, Integral i) => (Word8,Word8,Word8) -> i -> c -> c
storeBCDAt (h,t,o) i = setAll ram [(i,h), (i+1,t), (i+2,o)]

-- | store values in registers v(0..x) in ram(i..i+x)
dumpRegistersToMemory :: (Chip8 c, Integral i) => i -> c -> c
dumpRegistersToMemory x c = set ram (copyTo (mem,memOffset) (regs,0) length) c
  where 
    mem       = view ram c
    memOffset = int (view i c)
    regs      = view registers c
    length    = int x

-- | store values in ram(i..i+x) in registers v(0..x)
loadRegistersFromMemory :: (Chip8 c, Integral i) => i -> c -> c
loadRegistersFromMemory x c = set registers (copyTo (regs,0) (mem,memOffset) length) c
  where 
    mem       = view ram c
    memOffset = int (view i c)
    regs      = view registers c
    length    = int x

-- | render a sprite beginning at <x,y> with height 
renderSprite :: Chip8 c => Int -> Int -> Int -> c -> c
renderSprite x0 y0 height c = over display setDisplay c
  where
    ramOffset        = int (view i c)
    displayPixels    = view display c
    memory           = view ram c
    width            = 8
    offsets          = [0..(width-1)] × [0..(height-1)]
    setDisplay       = (// fmap writePixel offsets)
    writePixel (i,j) = (index,pixel)
      where
        x            = x0+i
        y            = y0+j
        index        = int (y*displayWidth+x)
        displayPixel = displayPixels ! index
        spritePixel  = nthbit (7-i) (memory ! (ramOffset+j))
        pixel        = displayPixel `xor` spritePixel
module Chip8 where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Array (Array, array, assocs, (!), (//))
import System.Random (StdGen, mkStdGen, genWord8)
import Lib
import Array2D
import UnsafeStack
import Chip8Architecture (Chip8Architecture(..))

data Chip8 s d = Chip8 {
  randomSeed :: StdGen,
  display    :: d,
  inputs     :: Array Word8 Bool,
  stack      :: s,
  v          :: Array Word8 Word8,
  d          :: Word8,
  s          :: Word8,
  pc         :: Word16,
  i          :: Word16,
  ram        :: Array Word16 Word8
} deriving Show

type Inputs = Array Word8 Bool
type Registers = Array Word8 Word8
type RAM = Array Word16 Word8
data C8Min s d 
  = C8Min StdGen d Inputs s Registers Word8 Word8 Word16 Word16 RAM
  deriving Show

data Display i e = Display {
  w      :: i,
  h      :: i,
  pixels :: Array i e
} deriving Show

instance Array2D Display where
  width            = w
  height           = h
  at v (x,y)       = pixels v ! (w v * y + x)
  update v pixels' = v { pixels = pixels v // fmap toFlatIndex pixels' }
    where
      toFlatIndex (x,y,e) = (w v * y + x,e)

instance UnsafeStack [] where
  push       = (:)  
  pop []     = error "Unsafe pop failure"
  pop (x:xs) = (x,xs)

instance Chip8Architecture Chip8 where
  randomSeed = Chip8.randomSeed
  setRandomSeed seed c8 = c8 { Chip8.randomSeed = seed }
  inputs = Chip8.inputs
  setInputs inputs c8 = c8 { Chip8.inputs = inputs }
  display = Chip8.display
  setDisplay display c8 = c8 { Chip8.display = display }
  stack = Chip8.stack
  setStack stack c8 = c8 { Chip8.stack = stack }
  v = Chip8.v
  setV v c8 = c8 { Chip8.v = v }
  d = Chip8.d
  setD d c8 = c8 { Chip8.d = d }
  s = Chip8.s
  setS s c8 = c8 { Chip8.s = s }
  pc = Chip8.pc
  setPC pc c8 = c8 { Chip8.pc = pc }
  i = Chip8.i
  setI i c8 = c8 { Chip8.i = i }
  ram = Chip8.ram
  setRAM ram c8 = c8 { Chip8.ram = ram }

seed :: StdGen -> Chip8 [Word16] (Display Word32 Bool)
seed randomSeed = Chip8 {
  Chip8.randomSeed = randomSeed,
  Chip8.display = Display { w = 64, h = 32, pixels = initialize (0,64 * 32 - 1) False },
  Chip8.inputs = initialize (0,0xF) False,
  Chip8.v = initialize (0,0xF) 0, 
  Chip8.stack = [],
  Chip8.ram = initialize (0,0xFFF) 0,
  Chip8.pc = 0x200,
  Chip8.d = 0,
  Chip8.s = 0,
  Chip8.i = 0
}
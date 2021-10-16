module Chip8Model where

import Prelude hiding (replicate)

import Control.Lens (Lens', lens)
import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector(..), replicate)
import System.Random (StdGen)

import Stack (Stack)
import Chip8 (Chip8(..))

data Chip8Model = Chip8Model {
  rand      :: StdGen,
  i         :: Word16,
  pc        :: Word16,
  s         :: Word8,
  d         :: Word8,
  stack     :: Stack Word16,
  registers :: Vector Word8,
  ram       :: Vector Word8,
  display   :: Vector Bool,
  inputs    :: Vector Bool
}

mkChip8 :: Vector Word8 -> Vector Word8 -> StdGen -> Chip8Model
mkChip8 font prog rand = load font prog rand Chip8Model {
  Chip8Model.rand      = rand,
  Chip8Model.i         = 0,
  Chip8Model.pc        = 0x200,
  Chip8Model.s         = 0,
  Chip8Model.d         = 0,
  Chip8Model.stack     = [],
  Chip8Model.registers = replicate 16 0,
  Chip8Model.ram       = replicate 4096 0,
  Chip8Model.display   = replicate (64 * 32) False,
  Chip8Model.inputs    = replicate 16 False
}

-- lenses could be generated via template haskell
instance Chip8 Chip8Model where
  rand      = lens Chip8Model.rand (\c8 rand -> c8 { Chip8Model.rand = rand })
  i         = lens Chip8Model.i (\c8 i -> c8 { Chip8Model.i = i })
  pc        = lens Chip8Model.pc (\c8 pc -> c8 { Chip8Model.pc = pc })
  s         = lens Chip8Model.s (\c8 s -> c8 { Chip8Model.s = s })
  d         = lens Chip8Model.d (\c8 d -> c8 { Chip8Model.d = d })
  stack     = lens Chip8Model.stack (\c8 stack -> c8 { Chip8Model.stack = stack })
  registers = lens Chip8Model.registers (\c8 registers -> c8 { Chip8Model.registers = registers })
  ram       = lens Chip8Model.ram (\c8 ram -> c8 { Chip8Model.ram = ram })
  display   = lens Chip8Model.display (\c8 display -> c8 { Chip8Model.display = display })
  inputs    = lens Chip8Model.inputs (\c8 inputs -> c8 { Chip8Model.inputs = inputs })
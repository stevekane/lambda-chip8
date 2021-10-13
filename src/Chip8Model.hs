module Chip8Model where

import Control.Lens (Lens', lens)
import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector(..))

import Chip8 

data Chip8Model = Chip8Model {
  i         :: Word16,
  pc        :: Word16,
  stack     :: [Word16],
  registers :: Vector Word8,
  ram       :: Vector Word8,
  display   :: Vector Bool
}

instance Chip8 Chip8Model where
  i         = lens Chip8Model.i (\c8 i -> c8 { Chip8Model.i = i })
  pc        = lens Chip8Model.pc (\c8 pc -> c8 { Chip8Model.pc = pc })
  stack     = lens Chip8Model.stack (\c8 stack -> c8 { Chip8Model.stack = stack })
  registers = lens Chip8Model.registers (\c8 registers -> c8 { Chip8Model.registers = registers })
  ram       = lens Chip8Model.ram (\c8 ram -> c8 { Chip8Model.ram = ram })
  display   = lens Chip8Model.display (\c8 display -> c8 { Chip8Model.display = display })
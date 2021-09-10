{-# LANGUAGE BinaryLiterals #-}

module Main where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Array (Array, array, (!), (//))
import Data.Word (Word8, Word16, Word32)

import Lib (highNibble, lowNibble, word8FromNibbles, word16FromNibbles, nthbit)

type RAMAddress = Word16
type StackAddress = Word8
type RegisterAddress = Word8
type DisplayAddress = Word32
type Pixel = Bool
type InputState = Bool
type Registers = Array RegisterAddress Word8

data Stack a = Stack {
  sp :: StackAddress,
  frames :: Array StackAddress a
} deriving (Show)

data Chip8 = Chip8 {
  inputs    :: Array Word8 InputState,        -- 16 booleans
  display   :: Array DisplayAddress Pixel,    -- 64 × 32 booleans indexed by a 32-bit Word
  d         :: Word8,                         -- byte delay timer
  s         :: Word8,                         -- byte sound timer
  registers :: Array RegisterAddress Word8,   -- 16 bytes
  stack     :: Stack RAMAddress,              -- stack of 2-byte adresses
  pc        :: RAMAddress,                    -- 2-byte program counter
  i         :: RAMAddress,                    -- 2-byte index register
  ram       :: Array RAMAddress Word8         -- 4096 bytes indexed by word16
} deriving (Show)

data OpCode
  -- display
  = Clear                                             -- 00E0
  | DrawVxVyN RegisterAddress RegisterAddress Word8   -- DXYN
  -- subroutine
  | Call RAMAddress                                   -- 2NNN
  | Return                                            -- 00EE
  -- jumps
  | JumpToNNN RAMAddress                              -- 1NNN
  | JumpToV0PlusNNN RAMAddress                        -- BNNN
  -- skips
  | SkipIfVxIsNN RegisterAddress Word8                -- 3XNN
  | SkipUnlessVxIsNN RegisterAddress Word8            -- 4XNN
  | SkipIfVxIsVy RegisterAddress RegisterAddress      -- 5XY0
  | SkipUnlessVxIsVy RegisterAddress RegisterAddress  -- 9XY0
  -- vx operations
  | SetVxToNN RegisterAddress Word8                   -- 6XNN
  | SetVxToVxPlusNN RegisterAddress Word8             -- 7XNN
  | SetVxToVy RegisterAddress RegisterAddress         -- 8XY0
  | SetVxToVxOrVy RegisterAddress RegisterAddress     -- 8XY1
  | SetVxToVxAndVy RegisterAddress RegisterAddress    -- 8XY2
  | SetVxToVxXorVy RegisterAddress RegisterAddress    -- 8XY3
  | SetVxToVxPlusVy RegisterAddress RegisterAddress   -- 8XY4
  | SetVxToVxMinusVy RegisterAddress RegisterAddress  -- 8XY5
  | SetVxToVyMinusVx RegisterAddress RegisterAddress  -- 8XY7
  | SetVxToRandomAndNN RegisterAddress Word8          -- CXNN
  | RightShiftVxByOne RegisterAddress                 -- 8XY6
  | LeftShiftVxByOne RegisterAddress                  -- 8XYE
  -- i operations
  | SetIToNNN RAMAddress                              -- ANNN
  | SetIToIPlusVx RegisterAddress                     -- FX1E
  | SetIToSpriteAddressVx RegisterAddress             -- FX29
  | StoreBCDVxAtI RegisterAddress                     -- FX33
  -- memory / register operations
  | DumpRegisters RegisterAddress                     -- FX55
  | LoadRegisters RegisterAddress                     -- FX65
  -- timers
  | SetVxToD RegisterAddress                          -- FX07
  | SetDToVx RegisterAddress                          -- FX15
  | SetSToVx RegisterAddress                          -- FX18
  -- input 
  | SkipIfKeyDownVx RegisterAddress                   -- EX9E
  | SkipUnlessKeyDownVx RegisterAddress               -- EXA1
  | BlockUnlessKeyDownVx RegisterAddress              -- FX0A
  deriving (Show)

off = False
on = True

initialize (min,max) v0 = array (min,max) [(i,v0) | i <- [min..max]]

load :: () -> Chip8
load program = Chip8 {
  inputs = initialize (0,15) off,
  display = initialize (0,64 * 32 - 1) off,
  d = 0,
  s = 0,
  registers = initialize (0,15) 0, 
  stack = Stack { sp = 0, frames = initialize (0,15) 0 },
  pc = 512,
  i = 0,
  ram = initialize (0,4095) 0
}

fetch :: Chip8 -> (Word8, Word8, Word8, Word8)
fetch c8 = 
  let b0 = ram c8 ! pc c8
      b1 = ram c8 ! (pc c8 + 1)
  in  (highNibble b0, lowNibble b0, highNibble b1, lowNibble b1)

at a x = a ! x
set a k v = a // [(k,v)]
word16 = fromIntegral
push s e = s { sp = sp s + 1, frames = set (frames s) (sp s) e }
peek s = frames s ! sp s
pop s = s { sp = sp s - 1 }
skipIf b = if b then 4 else 2

execute :: Chip8 -> Chip8
execute cpu = case fetch cpu of
  -- 2NNN Call subroutine at NNN
  (0x2, h, m, l) -> cpu { 
    stack = push (stack cpu) (pc cpu),
    pc = word16FromNibbles h m l 
  }
  -- 00EE Return from subroutine
  (0x0, 0x0, 0xE, 0xE) -> cpu {
    stack = pop (stack cpu),
    pc = peek (stack cpu)
  }

  -- 1NNN Jump to NNN
  (0x1, h, m, l) -> cpu { 
    pc = word16FromNibbles h m l
  }
  -- BNNN Jump to V0 + NNN
  (0xB, h, m, l) -> cpu { 
    pc = word16 (registers cpu `at` 0) + word16FromNibbles h m l
  }

  -- 3XNN Skip if Vx == NN
  (0x3, x, h, l) -> cpu {
    pc = pc cpu + skipIf (registers cpu `at` x == word8FromNibbles h l)
  }
  -- 4XNN Skip if Vx != NN
  (0x4, x, h, l) -> cpu {
    pc = pc cpu + skipIf (registers cpu `at` x /= word8FromNibbles h l)
  }
  -- 5XYN Skip if Vx == Vy
  (0x5, x, y, 0x0) -> cpu {
    pc = pc cpu + skipIf (registers cpu `at` x == registers cpu `at`)
  }
  -- 5XYN Skip if Vx == Vy
  (0x9, x, y, 0x0) -> cpu {
    pc = pc cpu + skipIf (registers cpu `at` x /= registers cpu `at y`)
  }

  -- UN-RECOGNIZED OPCODE
  _ -> cpu


chip8 = load ()

main :: IO ()
main = do
  print $ fetch chip8
  print $ word16FromNibbles 0xf 0xf 0xf
  print 0x0fff
  print $ pc $ execute (0x1, 0x1, 0x1, 0x1) chip8
  print $ pc $ execute (0xB, 0x1, 0x1, 0x1) chip8 { registers = registers chip8 // [(0,0xFF)] }
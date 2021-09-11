{-# LANGUAGE BinaryLiterals #-}

module Main where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Array (Array, array, (!), (//))
import Data.Word (Word8, Word16, Word32)
import System.Random 
import Lib

type RAMAddress = Word16
type StackAddress = Word8
type RegisterAddress = Word8
type DisplayAddress = Word32
type Pixel = Bool
type InputState = Bool
type Registers = Array RegisterAddress Word8

data Chip8 = Chip8 {
  randomSeed :: StdGen,
  inputs     :: Array Word8 InputState,        -- 16 booleans
  display    :: Array DisplayAddress Pixel,    -- 64 × 32 booleans indexed by a 32-bit Word
  d          :: Word8,                         -- byte delay timer
  s          :: Word8,                         -- byte sound timer
  pc         :: RAMAddress,                    -- 2-byte program counter
  sp         :: StackAddress,                  -- stack pointer
  i          :: RAMAddress,                    -- 2-byte index register
  registers  :: Array RegisterAddress Word8,   -- 16 bytes
  stack      :: Array StackAddress RAMAddress, -- stack of 2-byte adresses
  ram        :: Array RAMAddress Word8         -- 4096 bytes indexed by word16
} deriving (Show)

off = False
on = True

initialize (min,max) v0 = array (min,max) [(i,v0) | i <- [min..max]]

load :: StdGen -> () -> Chip8
load randomSeed program = Chip8 {
  randomSeed = randomSeed,
  inputs = initialize (0,15) off,
  display = initialize (0,64 * 32 - 1) off,
  d = 0,
  s = 0,
  registers = initialize (0,15) 0, 
  sp = 0, 
  stack = initialize (0,15) 0,
  pc = 512,
  i = 0,
  ram = initialize (0,4095) 0
}

fetch :: Chip8 -> (Word8, Word8, Word8, Word8)
fetch c8 = 
  let b0 = ram c8 ! pc c8
      b1 = ram c8 ! (pc c8 + 1)
  in  (highNibble b0, lowNibble b0, highNibble b1, lowNibble b1)

step pc = pc + 2
skipIf b = if b then 4 else 2
carry a b = if (word16 a + word16 b) > 255 then 1 else 0
borrow a b = if a > b then 0 else 1
lsb b = if nthbit 0 b then 1 else 0
msb b = if nthbit 7 b then 1 else 0

callSubroutineAtNNN nnn cpu = cpu { 
  pc = nnn, 
  sp = sp cpu + 1,
  stack = stack cpu // [(sp cpu,pc cpu)]
}
returnFromSubroutine cpu = cpu { 
  pc = stack cpu ! sp cpu,
  sp = sp cpu - 1
}
jumpToNNN nnn cpu = cpu { 
  pc = nnn 
}
jumpToV0PlusNNN v0 nnn cpu = cpu { 
  pc = word16 v0 + nnn 
}
skipIfVxIsNN vx nn cpu = cpu { 
  pc = pc cpu + skipIf (vx == nn) 
}
skipUnlessVxIsNN vx nn cpu = cpu { 
  pc = pc cpu + skipIf (vx /= nn) 
}
skipIfVxIsVy vx vy cpu = cpu { 
  pc = pc cpu + skipIf (vx == vy) 
}
skipUnlessVxIsVy vx vy cpu = cpu { 
  pc = pc cpu + skipIf (vx /= vy) 
}
setVxToNN x vx nn cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,nn)] 
}
setVxToVxPlusNN x vx nn cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx + nn)] 
}
setVxToVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vy)] 
}
setVxToVxOrVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx .|. vy)] 
}
setVxToVxAndVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx .&. vy)] 
}
setVxToVxXorVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx `xor` vy)] 
}
setVxToVxPlusVy x vx vy cpu = cpu {
  pc = step(pc cpu),
  registers = registers cpu // [(x, vx + vy), (0xF, carry vx vy)]
}
setVxToVxMinusVy x vx vy cpu = cpu {
  pc = step(pc cpu + 2),
  registers = registers cpu // [(x, vx - vy), (0xF, borrow vx vy)]
}
setVxToVyMinusVx x vx vy cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x, vy - vx), (0xF, borrow vy vx)]
}
leftShiftVxAndStoreLSBVx x vx cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x, vx `shiftR` 1), (0xF, lsb vx)]
}
rightShiftVxAndStoreMSBVx x vx cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x, vx `shiftL` 1), (0xF, msb vx)]
}
setVxToRandAndNN x nn cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x, randValue .&. nn)],
  randomSeed = randomSeed'
} where (randValue, randomSeed') = genWord8 (randomSeed cpu)

execute :: Chip8 -> Chip8
execute cpu = 
  let (a,x,y,z) = fetch cpu
      vx = registers cpu ! x
      vy = registers cpu ! y
      v0 = registers cpu ! 0x0
      nnn = word16FromNibbles x y z
      nn = word8FromNibbles y z
      n = z
  in  case (a,x,y,z) of
    (0x2, _, _, _)       -> callSubroutineAtNNN nnn cpu
    (0x0, 0x0, 0xE, 0xE) -> returnFromSubroutine cpu
    (0x1, _, _, _)       -> jumpToNNN nnn cpu
    (0xB, _, _, _)       -> jumpToV0PlusNNN v0 nnn cpu
    (0x3, x, _, _)       -> skipIfVxIsNN vx nn cpu
    (0x4, x, _, _)       -> skipUnlessVxIsNN vx nn cpu
    (0x5, x, y, 0x0)     -> skipIfVxIsVy vx vy cpu
    (0x9, x, y, 0x0)     -> skipUnlessVxIsVy vx vy cpu
    (0x6, x, _, _)       -> setVxToNN x vx nn cpu
    (0x7, x, _, _)       -> setVxToVxPlusNN x vx nn cpu
    (0x8, x, y, 0x0)     -> setVxToVy x vx vy cpu
    (0x8, x, y, 0x1)     -> setVxToVxOrVy x vx vy cpu
    (0x8, x, y, 0x2)     -> setVxToVxAndVy x vx vy cpu
    (0x8, x, y, 0x3)     -> setVxToVxXorVy x vx vy cpu
    (0x8, x, y, 0x4)     -> setVxToVxPlusVy x vx vy cpu
    (0x8, x, y, 0x5)     -> setVxToVxMinusVy x vx vy cpu
    (0x8, x, y, 0x7)     -> setVxToVyMinusVx x vx vy cpu
    (0x8, x, y, 0x6)     -> leftShiftVxAndStoreLSBVx x vx cpu
    (0x8, x, y, 0xE)     -> rightShiftVxAndStoreMSBVx x vx cpu
    (0xC, x, _, _)       -> setVxToRandAndNN x nn cpu
    -- (0x0,0x0,0xE,0x0)    -> clear cpu
    -- (0xD, _, _, _)       -> draw vx vy n cpu
    -- (0xA, _, _, _)       -> setIToNNN nnn cpu
    -- (0xF, _, 0x1, 0xE)   -> setIToIPlusVx x vx cpu
    -- (0xF, _, 0x2, 0x9)   -> setIToISpriteAddressVx x vx cpu
    -- (0xF, _, 0x3, 0x3)   -> storeBCDVxAtI vx cpu
    -- (0xF, _, 0x5, 0x5)   -> dumpRegistersV0ToVxToI x cpu
    -- (0xF, _, 0x6, 0x5)   -> loadRegistersV0ToVxFromI x cpu
    -- (0xF, _, 0x0, 0x7)   -> setVxToD x cpu
    -- (0xF, _, 0x1, 0x5)   -> setDToVx vx cpu
    -- (0xF, _, 0x1, 0x8)   -> setSToVx vx cpu
    -- (0xE, _, 0x9, 0xE)   -> skipIfKeyDownVx vx cpu
    -- (0xE, _, 0xA, 0x1)   -> skipUnlessKeyDownVx vx cpu
    -- (0xF, _, 0x0, 0xA)   -> blockUnlessKeyDownVx vx cpu
    _ -> cpu


rndSeed = mkStdGen 10
chip8 = load rndSeed ()

main :: IO ()
main = do
  print $ fetch chip8
  print $ word16FromNibbles 0xf 0xf 0xf
  print 0x0fff
  print $ pc $ execute chip8
  -- print $ pc $ registers (chip8 { registers = registers chip8 `setting` [(0,0xFF)] }) `at` 0
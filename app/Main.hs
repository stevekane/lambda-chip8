module Main where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Array (Array, array, (!), (//))
import Data.Word (Word8, Word16, Word32)
import System.Random 
import Lib

-- Type aliases
type RAMAddress = Word16
type StackAddress = Word8
type RegisterAddress = Word8
type DisplayAddress = Word32
type Pixel = Bool
type InputState = Bool
type Registers = Array RegisterAddress Word8
type Nibbles = (Word8, Word8, Word8, Word8)
type OpCode = (Word8, Word8, Word8, Word8, Word8, Word8, Word16)

-- Model for Chip8 CPU
data Chip8 = Chip8 {
  randomSeed :: StdGen,                        -- seeded generator for pseudo-random bytes
  display    :: Array DisplayAddress Pixel,    -- 64 × 32 booleans indexed by a 32-bit Word
  inputs     :: Array Word8 InputState,        -- 16 booleans
  d          :: Word8,                         -- byte delay timer
  s          :: Word8,                         -- byte sound timer
  pc         :: RAMAddress,                    -- 2-byte program counter
  sp         :: StackAddress,                  -- 1-byte stack pointer
  i          :: RAMAddress,                    -- 2-byte index register
  registers  :: Array RegisterAddress Word8,   -- 16 bytes
  stack      :: Array StackAddress RAMAddress, -- 16 2-byte adresses
  ram        :: Array RAMAddress Word8         -- 4096 bytes indexed by word16
} deriving (Show)

{-
Think about the actual API here... what are fetch decode and execute really?

It doesn't seem like the thing I am calling an OpCode really is one... it's just
a bag of commonly-used variables extracted from registers or assembled from nibbles.

* Clean this up and start adding tests for all the key operations.
* Implement drawing algorithm
* Add built-on font data as binary loaded into RAM
* Read binary data from file and load into RAM as program at 512
* Implement simple console renderer to test IBM logo and other similar programs
-}

-- Constants
instructionByteWidth = 2
displayWidth = 64
displayHeight = 32
off = False
on = True
blankDisplay = initialize (0,displayWidth * displayHeight - 1) off

-- Semantic helper functions
step pc = pc + instructionByteWidth

skipIf pc True = pc + instructionByteWidth + instructionByteWidth
skipIf pc False = pc + instructionByteWidth

blockIf pc True = pc
blockIf pc False = pc + instructionByteWidth

initialize (min,max) v0 = array (min,max) (fmap initialPair [min..max])
  where initialPair i = (i,v0)

copyTo (to,t0) (from,f0) n = to // fmap updatePair [0..n]
  where updatePair i = (t0 + fromIntegral i,from ! (f0 + fromIntegral i))

-- OpCode transformations
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
  pc = skipIf (pc cpu) (vx == nn) 
}

skipUnlessVxIsNN vx nn cpu = cpu { 
  pc = skipIf (pc cpu) (vx /= nn) 
}

skipIfVxIsVy vx vy cpu = cpu { 
  pc = skipIf (pc cpu) (vx == vy) 
}

skipUnlessVxIsVy vx vy cpu = cpu { 
  pc = skipIf (pc cpu) (vx /= vy) 
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
  pc = step (pc cpu),
  registers = registers cpu // [(x,sum), (0xF,toWord8 carry)]
} where (sum, carry) = vx `addWithCarry` vy

setVxToVxMinusVy x vx vy cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,difference), (0xF,toWord8 (not borrow))]
} where (difference, borrow) = vx `subtractWithBorrow` vy

setVxToVyMinusVx x vx vy cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,difference), (0xF,toWord8 (not borrow))]
} where (difference, borrow) = vy `subtractWithBorrow` vx

rightShiftVxAndStoreLSBVx x vx cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,vx `shiftR` 1), (0xF,lsbvx)]
} where lsbvx = toWord8 (nthbit 0 vx)

leftShiftVxAndStoreMSBVx x vx cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,vx `shiftL` 1), (0xF,msbvx)]
} where msbvx = toWord8 (nthbit 7 vx)

setVxToRandAndNN x nn cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,randValue .&. nn)],
  randomSeed = randomSeed'
} where (randValue, randomSeed') = genWord8 (randomSeed cpu)

clearDisplay cpu = cpu {
  pc = step (pc cpu),
  display = blankDisplay
}

-- Fuck this function. This is so complex and ugly.
drawSpriteAtIToVxVyNHigh vx vy n cpu = cpu {
  pc = step (pc cpu),
  display = display cpu // pixels,
  registers = registers cpu // [(0xF,toWord8 collisionFlag)]
} where
    x = word32 vx `mod` displayWidth
    y = word32 vy `mod` displayHeight
    xMax = max (x + 8) displayWidth - 1
    yMax = max (y + word32 n) displayHeight - 1
    iMax = xMax - x
    jMax = yMax - y
    offsets = [0..iMax] × [0..jMax]
    ramIndex = i cpu
    (pixels,collisionFlag) = foldr pixelWrite ([],False) offsets
    pixelWrite (i,j) (pixels,collisionFlag) = (pixels',collisionFlag')
      where
        displayIndex = to1DIndex displayWidth (x + i, y + j)
        displayValue = display cpu ! displayIndex
        spriteWord = ram cpu ! (ramIndex + word16 j)
        spriteValue = nthbit i spriteWord
        pixels' = (displayIndex,spriteValue) : pixels
        collisionFlag' = collisionFlag || (spriteValue `xor` displayValue)

setIToNNN nnn cpu = cpu {
  pc = step (pc cpu),
  i = nnn
}

setIToIPlusVx vx cpu = cpu {
  pc = step (pc cpu),
  i = i cpu + word16 vx
}

setIToISpriteAddressVx vx cpu = cpu {
  pc = step (pc cpu),
  i = word16 vx * fontHeight
} where fontHeight = 5

storeBCDVxAtI vx cpu = cpu {
  pc = step (pc cpu),
  ram = ram cpu // [(i cpu,hundreds), (i cpu + 1,tens), (i cpu + 2,ones)]
} where (hundreds, tens, ones) = digits vx

dumpRegistersV0ToVxToI x cpu = cpu {
  pc = step (pc cpu),
  ram = copyTo (ram cpu,i cpu) (registers cpu,0) x
}

loadRegistersV0ToVxFromI x cpu = cpu {
  pc = step (pc cpu),
  registers = copyTo (registers cpu,0) (ram cpu,i cpu) x
}

setVxToD x cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,d cpu)]
}

setDToVx vx cpu = cpu {
  pc = step (pc cpu),
  d = vx
}

setSToVx vx cpu = cpu {
  pc = step (pc cpu),
  s = vx
}

skipIfKeyDownVx vx cpu = cpu {
  pc = skipIf (pc cpu) (inputs cpu ! vx) 
}

skipUnlessKeyDownVx vx cpu = cpu {
  pc = skipIf (pc cpu) (not (inputs cpu ! vx))
}

blockUnlessKeyDownVx vx cpu = cpu {
  pc = blockIf (pc cpu) (not (inputs cpu ! vx))
}

-- Construct new CPU with random seed and program
load :: StdGen -> () -> Chip8
load randomSeed program = Chip8 {
  randomSeed = randomSeed,
  display = blankDisplay,
  inputs = initialize (0,15) off,
  registers = initialize (0,15) 0, 
  stack = initialize (0,15) 0,
  ram = initialize (0,4095) 0,
  pc = 512,
  sp = 0, 
  d = 0,
  s = 0,
  i = 0
}

-- fetch two bytes from cpu at current pc and split into 4 nibbles
fetch :: Chip8 -> Nibbles
fetch cpu = (highNibble b0, lowNibble b0, highNibble b1, lowNibble b1)
  where 
    b0 = ram cpu ! pc cpu
    b1 = ram cpu ! (pc cpu + 1)

-- extract commonly-used values from cpu registers and instruction nibbles
decode :: Nibbles -> Chip8 -> OpCode
decode (_,x,y,z) cpu = (x,vx,vy,v0,n,nn,nnn)
  where
    x = x
    vx = registers cpu ! x
    vy = registers cpu ! y
    v0 = registers cpu ! 0x0
    nnn = word16FromNibbles x y z
    nn = word8FromNibbles y z
    n = z
  
-- fetch, decode and execute the next opcode
execute :: Chip8 -> Chip8
execute cpu = 
  let nibbles = fetch cpu
      (x,vx,vy,v0,n,nn,nnn) = decode nibbles cpu
  in  case nibbles of
    (0x2, _, _, _)       -> callSubroutineAtNNN nnn cpu
    (0x0, 0x0, 0xE, 0xE) -> returnFromSubroutine cpu
    (0x1, _, _, _)       -> jumpToNNN nnn cpu
    (0xB, _, _, _)       -> jumpToV0PlusNNN v0 nnn cpu
    (0x3, _, _, _)       -> skipIfVxIsNN vx nn cpu
    (0x4, _, _, _)       -> skipUnlessVxIsNN vx nn cpu
    (0x5, _, _, 0x0)     -> skipIfVxIsVy vx vy cpu
    (0x9, _, _, 0x0)     -> skipUnlessVxIsVy vx vy cpu
    (0x6, _, _, _)       -> setVxToNN x vx nn cpu
    (0x7, _, _, _)       -> setVxToVxPlusNN x vx nn cpu
    (0x8, _, _, 0x0)     -> setVxToVy x vx vy cpu
    (0x8, _, _, 0x1)     -> setVxToVxOrVy x vx vy cpu
    (0x8, _, _, 0x2)     -> setVxToVxAndVy x vx vy cpu
    (0x8, _, _, 0x3)     -> setVxToVxXorVy x vx vy cpu
    (0x8, _, _, 0x4)     -> setVxToVxPlusVy x vx vy cpu
    (0x8, _, _, 0x5)     -> setVxToVxMinusVy x vx vy cpu
    (0x8, _, _, 0x7)     -> setVxToVyMinusVx x vx vy cpu
    (0x8, _, _, 0x6)     -> rightShiftVxAndStoreLSBVx x vx cpu
    (0x8, _, _, 0xE)     -> leftShiftVxAndStoreMSBVx x vx cpu
    (0xC, _, _, _)       -> setVxToRandAndNN x nn cpu
    (0x0,0x0,0xE,0x0)    -> clearDisplay cpu
    (0xD, _, _, _)       -> drawSpriteAtIToVxVyNHigh vx vy n cpu
    (0xA, _, _, _)       -> setIToNNN nnn cpu
    (0xF, _, 0x1, 0xE)   -> setIToIPlusVx vx cpu
    (0xF, _, 0x2, 0x9)   -> setIToISpriteAddressVx vx cpu
    (0xF, _, 0x3, 0x3)   -> storeBCDVxAtI vx cpu
    (0xF, _, 0x5, 0x5)   -> dumpRegistersV0ToVxToI x cpu
    (0xF, _, 0x6, 0x5)   -> loadRegistersV0ToVxFromI x cpu
    (0xF, _, 0x0, 0x7)   -> setVxToD x cpu
    (0xF, _, 0x1, 0x5)   -> setDToVx vx cpu
    (0xF, _, 0x1, 0x8)   -> setSToVx vx cpu
    (0xE, _, 0x9, 0xE)   -> skipIfKeyDownVx vx cpu
    (0xE, _, 0xA, 0x1)   -> skipUnlessKeyDownVx vx cpu
    (0xF, _, 0x0, 0xA)   -> blockUnlessKeyDownVx vx cpu
    _                    -> cpu

rndSeed = mkStdGen 10
chip8 = load rndSeed ()

main :: IO ()
main = do
  print $ digits 0xf3
  print $ fetch chip8
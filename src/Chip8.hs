module Chip8 where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Array (Array, array, assocs, (!), (//))
import System.Random (StdGen, mkStdGen, genWord8)
import Lib
import Array2D
import UnsafeStack

type RAMAddress = Word16
type StackAddress = Word8
type RegisterAddress = Word8
type InputState = Bool
type Registers = Array RegisterAddress Word8
type Stack = [RAMAddress]
type RAM = Array RAMAddress Word8
type ROM = Array RAMAddress Word8
type Font = Array RAMAddress Word8
type Nibbles = (Word8, Word8, Word8, Word8)
type OpCode = (Word8, Word8, Word8, Word8, Word8, Word8, Word16)

data Chip8 = Chip8 {
  randomSeed :: StdGen,
  display    :: Display Word32 Bool,
  inputs     :: Array Word8 InputState,
  stack      :: Stack,
  v          :: Array RegisterAddress Word8,
  d          :: Word8,
  s          :: Word8,
  pc         :: RAMAddress,
  i          :: RAMAddress,
  ram        :: Array RAMAddress Word8
} deriving Show

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

callSubroutineAtNNN nnn cpu = cpu { 
  pc = nnn, 
  stack = push (pc cpu + 2) (stack cpu)
}

returnFromSubroutine cpu = cpu { 
  pc = pc',
  stack = stack'
} where (pc',stack') = pop (stack cpu)

jumpToNNN nnn cpu = cpu { 
  pc = nnn 
}

jumpToV0PlusNNN v0 nnn cpu = cpu { 
  pc = word16 v0 + nnn 
}

skipIfVxIsNN vx nn cpu = cpu { 
  pc = pc cpu + if vx == nn then 4 else 2
}

skipUnlessVxIsNN vx nn cpu = cpu { 
  pc = pc cpu + if vx /= nn then 4 else 2
}

skipIfVxIsVy vx vy cpu = cpu { 
  pc = pc cpu + if vx == vy then 4 else 2
}

skipUnlessVxIsVy vx vy cpu = cpu { 
  pc = pc cpu + if vx /= vy then 4 else 2
}

setVxToNN x vx nn cpu = cpu { 
  pc = pc cpu + 2, 
  v = v cpu // [(x,nn)] 
}

setVxToVxPlusNN x vx nn cpu = cpu { 
  pc = pc cpu + 2, 
  v = v cpu // [(x,vx + nn)] 
}

setVxToVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  v = v cpu // [(x,vy)] 
}

setVxToVxOrVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  v = v cpu // [(x,vx .|. vy)] 
}

setVxToVxAndVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  v = v cpu // [(x,vx .&. vy)] 
}

setVxToVxXorVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  v = v cpu // [(x,vx `xor` vy)] 
}

setVxToVxPlusVy x vx vy cpu = cpu {
  pc = pc cpu + 2,
  v = v cpu // [(x,sum), (0xF,toWord8 carry)]
} where (sum, carry) = vx `addWithCarry` vy

setVxToVxMinusVy x vx vy cpu = cpu {
  pc = pc cpu + 2,
  v = v cpu // [(x,difference), (0xF,toWord8 . not $ borrow)]
} where (difference, borrow) = vx `subtractWithBorrow` vy

setVxToVyMinusVx x vx vy cpu = cpu {
  pc = pc cpu + 2,
  v = v cpu // [(x,difference), (0xF,toWord8 . not $ borrow)]
} where (difference, borrow) = vy `subtractWithBorrow` vx

rightShiftVxAndStoreLSBVx x vx cpu = cpu {
  pc = pc cpu + 2,
  v = v cpu // [(x,vx `shiftR` 1), (0xF,lsbvx)]
} where lsbvx = toWord8 (nthbit 0 vx)

leftShiftVxAndStoreMSBVx x vx cpu = cpu {
  pc = pc cpu + 2,
  v = v cpu // [(x,vx `shiftL` 1), (0xF,msbvx)]
} where msbvx = toWord8 (nthbit 7 vx)

setVxToRandAndNN x nn cpu = cpu {
  pc = pc cpu + 2,
  v = v cpu // [(x,randValue .&. nn)],
  randomSeed = randomSeed'
} where (randValue, randomSeed') = genWord8 . randomSeed $ cpu

clearDisplay cpu = cpu {
  pc = pc cpu + 2,
  display = display cpu `fill` False
}

drawSpriteAtIToVxVyNHigh :: Word8 -> Word8 -> Word8 -> Chip8 -> Chip8
drawSpriteAtIToVxVyNHigh vx vy n cpu = cpu {
  pc = pc cpu + 2,
  display = display cpu `update` pixels
} where
  (x0,y0) = (word32 vx, word32 vy)
  (w,h)   = (8,word32 n)
  offsets = [0..(w-1)] × [0..(h-1)]
  ramOffset = i cpu
  pixels = fmap writePixel offsets
  writePixel (i,j) = (x,y,pixel)
    where
      (x,y) = (x0 + i, y0 + j)
      displayPixel = display cpu `at` (x,y)
      spritePixel = nthbit (7 - i) (ram cpu ! (ramOffset + word16 j))
      pixel = displayPixel `xor` spritePixel

setIToNNN nnn cpu = cpu {
  pc = pc cpu + 2,
  i = nnn
}

setIToIPlusVx vx cpu = cpu {
  pc = pc cpu + 2,
  i = i cpu + word16 vx
}

setIToISpriteAddressVx vx cpu = cpu {
  pc = pc cpu + 2,
  i = word16 vx * fontHeight
} where fontHeight = 5

storeBCDVxAtI vx cpu = cpu {
  pc = pc cpu + 2,
  ram = ram cpu // [(i cpu,hundreds), (i cpu + 1,tens), (i cpu + 2,ones)]
} where (hundreds, tens, ones) = digits vx

dumpRegistersV0ToVxToI x cpu = cpu {
  pc = pc cpu + 2,
  ram = copyTo (ram cpu,i cpu) (v cpu,0) x
}

loadRegistersV0ToVxFromI x cpu = cpu {
  pc = pc cpu + 2,
  v = copyTo (v cpu,0) (ram cpu,i cpu) x
}

setVxToD x cpu = cpu {
  pc = pc cpu + 2,
  v = v cpu // [(x,d cpu)]
}

setDToVx vx cpu = cpu {
  pc = pc cpu + 2,
  d = vx
}

setSToVx vx cpu = cpu {
  pc = pc cpu + 2,
  s = vx
}

skipIfKeyDownVx vx cpu = cpu {
  pc = pc cpu + if inputs cpu ! vx then 4 else 2
}

skipUnlessKeyDownVx vx cpu = cpu {
  pc = pc cpu + if inputs cpu ! vx then 2 else 4
}

blockUnlessKeyDownVx vx cpu = cpu {
  pc = pc cpu + if inputs cpu ! vx then 2 else 0
}

decrementTimers :: Chip8 -> Chip8
decrementTimers cpu = cpu {
  s = max 0 (s cpu - 1),
  d = max 0 (d cpu - 1)
}

seed :: StdGen -> Chip8
seed randomSeed = Chip8 {
  randomSeed = randomSeed,
  display = Display { w = 64, h = 32, pixels = initialize (0,64 * 32 - 1) False },
  inputs = initialize (0,0xF) False,
  v = initialize (0,0xF) 0, 
  stack = [],
  ram = initialize (0,0xFFF) 0,
  pc = 0x200,
  d = 0,
  s = 0,
  i = 0
}

loadFont :: Font -> Chip8 -> Chip8
loadFont font cpu = cpu {
  ram = ram cpu // assocs font
}

loadProgram :: RAM -> Chip8 -> Chip8
loadProgram p cpu = cpu {
  pc = 0x200,
  ram = ram cpu // fmap (shiftBy 0x200) (assocs p)
} where shiftBy o (i,j) = (i + o,j)

fetch :: Chip8 -> Nibbles
fetch cpu = (highNibble b0, lowNibble b0, highNibble b1, lowNibble b1)
  where 
    b0 = ram cpu ! pc cpu
    b1 = ram cpu ! (pc cpu + 1)

execute :: Nibbles -> Chip8 -> Chip8
execute (a,x,y,n) cpu = case (a,x,y,n) of 
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
  (0xF, _, 0x5, 0x5)   -> dumpRegistersV0ToVxToI (fromIntegral x) cpu
  (0xF, _, 0x6, 0x5)   -> loadRegistersV0ToVxFromI x cpu
  (0xF, _, 0x0, 0x7)   -> setVxToD x cpu
  (0xF, _, 0x1, 0x5)   -> setDToVx vx cpu
  (0xF, _, 0x1, 0x8)   -> setSToVx vx cpu
  (0xE, _, 0x9, 0xE)   -> skipIfKeyDownVx vx cpu
  (0xE, _, 0xA, 0x1)   -> skipUnlessKeyDownVx vx cpu
  (0xF, _, 0x0, 0xA)   -> blockUnlessKeyDownVx vx cpu
  _                    -> error $ "unknown opcode: " ++ show (a,x,y,n)
  where 
  vx = v cpu ! x
  vy = v cpu ! y
  v0 = v cpu ! 0x0
  nnn = word16FromNibbles x y n
  nn = word8FromNibbles y n
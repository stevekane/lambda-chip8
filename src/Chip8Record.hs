{-# LANGUAGE RankNTypes #-}

module Chip8Record where

import Prelude hiding (replicate)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Control.Lens (Lens'(..), ASetter(..), lens, set, over, view)
import Data.Vector (Vector(..), (!), (//), replicate)

import Lib

data Chip8Record c = Chip8Record {
  push    :: Word16 -> c -> c,
  pop     :: c -> (Word16, c),
  i       :: Lens' c Word16,
  pc      :: Lens' c Word16,
  ram     :: Lens' c (Vector Word8),
  v       :: Lens' c (Vector Word8),
  d       :: Lens' c Word8,
  s       :: Lens' c Word8,
  inputs  :: Lens' c (Vector Bool),
  display :: Lens' c (Vector Bool)
}

data C8Classic = C8Classic {
  indexPointer   :: Word16,
  programCounter :: Word16,
  memory         :: Vector Word8,
  stackPointer   :: Word8,
  stackFrames    :: Vector Word16,
  registers      :: Vector Word8,
  delayTimer     :: Word8,
  soundTimer     :: Word8,
  inputBuffer    :: Vector Bool,
  displayBuffer  :: Vector Bool
}

chip8Record = Chip8Record {
  push = \w s -> 
    let sp = stackPointer s 
    in  s { 
      stackPointer = sp + 1, 
      stackFrames = stackFrames s // [(fromIntegral sp,w)] 
    },
  pop = \s -> 
    let sp  = stackPointer s 
        sp' = sp - 1
        w   = stackFrames s ! fromIntegral sp'
        s'  = s { stackPointer = sp' }
    in  (w,s'),
  i = lens indexPointer (\c8 i -> c8 { indexPointer = i }),
  pc = lens programCounter (\c8 pc -> c8 { programCounter = pc }),
  ram = lens memory (\c8 ram -> c8 { memory = ram }),
  v = lens registers (\c8 v -> c8 { registers = v }),
  d = lens delayTimer (\c8 d -> c8 { delayTimer = d }),
  s = lens soundTimer (\c8 s -> c8 { soundTimer = s }),
  inputs = lens inputBuffer (\c8 inputs -> c8 { inputBuffer = inputs }),
  display = lens displayBuffer (\c8 display -> c8 { displayBuffer = display })
}

inc :: Integral b => ASetter s t b b -> b -> s -> t
inc l v = over l (+v)

fetchNibbles :: Chip8Record c -> c -> (Word8,Word8,Word8,Word8)
fetchNibbles r c8 = (highNibble b0, lowNibble b0, highNibble b1, lowNibble b1)
  where
    pc0 = fromIntegral (view (pc r) c8)
    pc1 = pc0 + 1
    memory = view (ram r) c8
    b0 = memory ! pc0
    b1 = memory ! pc1

execute :: Chip8Record c -> c -> c
execute r c8 = case (a,x,y,z) of 
  -- clear display
  (0x0, 0x0, 0xE, 0x0) -> inc (pc r) 2 . set (display r) blankDisplay $ c8
    where
      size         = 64 * 32
      blankDisplay = replicate size False

  -- pc = pop stack
  (0x0, 0x0, 0xE, 0xE) -> set (pc r) pc' c8'
    where 
      (pc',c8') = pop r c8
  
  -- pc = nnn
  (0x1, _, _, _) -> set (pc r) nnn c8

  -- pc = nnn. push (pc + 2)
  (0x2, _, _, _) -> set (pc r) nnn . push r pc' $ c8
    where
      pc' = 2 + view (pc r) c8

  -- step pc. v(x) = nn
  (0x6, _, _, _) -> inc (pc r) 2 . over (v r) setVx $ c8
    where
      setVx r = r // [(fromIntegral x,nn)]
    
  (0x7, _, _, _) -> inc (pc r) 2 . over (v r) setVx $ c8
    where
      setVx r = r // [(fromIntegral x, nn + vx)]

  -- step pc. i = nnn
  (0xA, _, _, _) -> inc (pc r) 2 . set (i r) nnn $ c8

  -- step pc. draw sprite at v(x) v(y) of height n
  (0xD, _, _, _) -> inc (pc r) 2 . over (display r) setDisplay $ c8
    where
      (x0,y0)          = (fromIntegral vx,fromIntegral vy)
      (w,h)            = (8,fromIntegral z)
      offsets          = [0..(w-1)] × [0..(h-1)]
      ramOffset        = fromIntegral (view (i r) c8)
      displayPixels    = view (display r) c8
      memory           = view (ram r) c8
      setDisplay d     = d // fmap writePixel offsets
      writePixel (i,j) = (index,pixel)
        where
          (x,y)        = (x0 + i, y0 + j)
          index        = fromIntegral (y * 64 + x)
          displayPixel = displayPixels ! index
          spritePixel  = nthbit (7 - i) (memory ! (ramOffset + j))
          pixel        = displayPixel `xor` spritePixel

  _ -> error $ "Unknown opcode: "  ++ show (a,x,y,z)
  where
    (a,x,y,z) = fetchNibbles r c8
    vx = view (v r) c8 ! fromIntegral x
    vy = view (v r) c8 ! fromIntegral y
    v0 = view (v r) c8 ! 0x0
    nnn = word16FromNibbles x y z
    nn = word8FromNibbles y z
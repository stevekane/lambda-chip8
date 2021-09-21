module Chip8Architecture where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Array (Array, array, assocs, (!), (//))
import Lib
import Array2D
import UnsafeStack
import System.Random (StdGen, genWord8)

class Chip8Architecture c where
  randomSeed     :: c s d -> StdGen
  setRandomSeed  :: StdGen -> c s d -> c s d
  inputs         :: c s d -> Array Word8 Bool
  setInputs      :: Array Word8 Bool -> c s d -> c s d
  display        :: Array2D d => c s (d Word32 Bool) -> d Word32 Bool
  setDisplay     :: Array2D d => d Word32 Bool -> c s (d Word32 Bool) -> c s (d Word32 Bool)
  stack          :: UnsafeStack s => c (s Word16) d -> s Word16
  setStack       :: UnsafeStack s => s Word16 -> c (s Word16) d -> c (s Word16) d
  v              :: c s d -> Array Word8 Word8
  setV           :: Array Word8 Word8 -> c s d -> c s d
  d              :: c s d -> Word8
  setD           :: Word8 -> c s d -> c s d
  s              :: c s d -> Word8
  setS           :: Word8 -> c s d -> c s d
  pc             :: c s d -> Word16 
  setPC          :: Word16 -> c s d -> c s d
  i              :: c s d -> Word16 
  setI           :: Word16 -> c s d -> c s d
  ram            :: c s d -> Array Word16 Word8
  setRAM         :: Array Word16 Word8 -> c s d -> c s d

  loadProgram :: Array Word16 Word8 -> c s d -> c s d
  loadProgram p c8 = setPC 0x200 . setRAM ram' $ c8
    where 
      shiftBy o (i,j) = (i + o,j)
      ram' = ram c8 // fmap (shiftBy 0x200) (assocs p)

  loadFont :: Array Word16 Word8 -> c s d -> c s d
  loadFont f c8 = setRAM ram' c8
    where ram' = ram c8 // assocs f

  decrementTimers :: c s d -> c s d
  decrementTimers c8 = setS (s c8 - 1) . setD (d c8 - 1) $ c8

  stepPC :: c s d -> c s d
  stepPC c8 = setPC (pc c8 + 2) c8

  skipPC :: c s d -> c s d
  skipPC c8 = setPC (pc c8 + 4) c8

  fetchNibbles :: c s d -> (Word8,Word8,Word8,Word8)
  fetchNibbles c8 = (highNibble b0, lowNibble b0, highNibble b1, lowNibble b1)
    where 
      b0 = ram c8 ! pc c8
      b1 = ram c8 ! (pc c8 + 1)

  execute :: 
    (UnsafeStack s, Array2D d) => 
    c (s Word16) (d Word32 Bool) -> 
    c (s Word16) (d Word32 Bool)
  execute c8 = case (a,x,y,z) of
    -- pc = nnn. stack push pc + 2
    (0x2, _, _, _) -> setStack stack' . setPC nnn $ c8
      where 
        stack' = push (pc c8 + 2) (stack c8)

    -- pc = pop stack
    (0x0, 0x0, 0xE, 0xE) -> setPC pc . setStack stack' $ c8
      where 
        (pc,stack') = pop (stack c8)

    -- pc = nnn
    (0x1, _, _, _) -> setPC nnn c8

    -- pc = v0 + NNN
    (0xB, _, _, _) -> setPC (word16 v0 + nnn) c8

    -- skip if v(x) == nn
    (0x3, _, _, _) -> if vx == nn then skipPC c8 else stepPC c8

    -- skip unless v(x) == nn
    (0x4, _, _, _) -> if vx /= nn then skipPC c8 else stepPC c8

    -- skip if v(x) == v(y)
    (0x5, _, _, _) -> if vx == vy then skipPC c8 else stepPC c8

    -- skip unless v(x) == v(y)
    (0x9, _, _, _) -> if vx /= vy then skipPC c8 else stepPC c8

    -- step pc. v(x) = nn
    (0x6, _, _, _) -> stepPC . setV v' $ c8
      where 
        v' = v c8 // [(x,nn)]

    -- step pc. v(x) = v(x) + nn
    (0x7, _, _, _) -> stepPC . setV v' $ c8
      where 
        v' = v c8 // [(x,vx + nn)]

    -- step pc. v(x) = v(y)
    (0x8, _, _, 0x0) -> stepPC . setV v' $ c8
      where 
        v' = v c8 // [(x,vy)]

    -- step pc. v(x) = v(x) | v(y)
    (0x8, _, _, 0x1) -> stepPC . setV v' $ c8
      where 
        v' = v c8 // [(x,vx .|. vy)]

    -- step pc. v(x) = v(x) & v(y)
    (0x8, _, _, 0x2) -> stepPC . setV v' $ c8
      where 
        v' = v c8 // [(x,vx .&. vy)]

    -- step pc. v(x) = v(x) `xor` v(y)
    (0x8, _, _, 0x3) -> stepPC . setV v' $ c8
      where 
        v' = v c8 // [(x,vx `xor` vy)]

    -- step pc. v(x) = v(x) + v(y). v(f) = carry
    (0x8, _, _, 0x4) -> stepPC . setV v' $ c8
      where 
        (sum,carry) = vx `addWithCarry` vy
        v' = v c8 // [(x,sum), (0xF,toWord8 carry)]

    -- step pc. v(x) = v(x) - v(y). v(f) = not borrow
    (0x8, _, _, 0x5) -> stepPC . setV v' $ c8
      where
        (difference,borrow) = vx `subtractWithBorrow` vy
        v' = v c8 // [(x,difference), (0xF,toWord8 . not $ borrow)]

    -- step pc. v(x) = v(y) - v(x). v(f) = not borrow
    (0x8, _, _, 0x7) -> stepPC . setV v' $ c8
      where
        (difference,borrow) = vy `subtractWithBorrow` vx
        v' = v c8 // [(x,difference), (0xF,toWord8 . not $ borrow)]

    -- step pc. v(x) = v(x) >> 1. v(f) = LSB
    (0x8, _, _, 0x6) -> stepPC . setV v' $ c8
      where 
        lsbvx = toWord8 . nthbit 0 $ vx
        v' = v c8 // [(x,vx `shiftR` 1), (0xF,lsbvx)]

    -- step pc. v(x) = v(x) << 1. v(f) = MSB
    (0x8, _, _, 0xE) -> stepPC . setV v' $ c8
      where 
        msbvx = toWord8 . nthbit 7 $ vx
        v' = v c8 // [(x,vx `shiftL` 1), (0xF,msbvx)]

    -- step pc. v(x) = Rand & nn
    (0xC, _, _, _) -> stepPC . setV v' . setRandomSeed randomSeed' $ c8
      where
        (randomWord8,randomSeed') = genWord8 . randomSeed $ c8 
        v' = v c8 // [(x,randomWord8 .&. nn)]

    -- step pc. clear display
    (0x0, 0x0, 0xE, 0x0) -> stepPC . setDisplay (display c8 `fill` False) $ c8

    -- step pc. draw sprite at v(x) v(y) of height n
    (0xD, _, _, _) -> stepPC . setDisplay display' $ c8
      where
        (x0,y0) = (word32 vx, word32 vy)
        (w,h)   = (8,word32 z)
        offsets = [0..(w-1)] × [0..(h-1)]
        ramOffset = i c8
        display' = display c8 `update` fmap writePixel offsets
        writePixel (i,j) = (x,y,pixel)
          where
            (x,y) = (x0 + i, y0 + j)
            displayPixel = display c8 `at` (x,y)
            spritePixel = nthbit (7 - i) (ram c8 ! (ramOffset + word16 j))
            pixel = displayPixel `xor` spritePixel
    
    -- step pc. i = nnn
    (0xA, _, _, _) -> stepPC . setI nnn $ c8

    -- step pc. i = i + v(x)
    (0xF, _, 0x1, 0xE) -> stepPC . setI i' $ c8
      where 
        i' = i c8 + word16 vx

    -- step pc. i = built-in-sprite-address(v(x))
    (0xF, _, 0x2, 0x9) -> stepPC . setI i' $ c8
      where 
        builtinFontHeight = 5
        i' = word16 vx * builtinFontHeight

    -- step pc. ram[i] = hundreds(v(x)). ram[i+1] = tens(v(x)). ram[i+2] = ones(v(x))
    (0xF, _, 0x3, 0x3) -> stepPC . setRAM ram' $ c8 
      where
        (hundreds,tens,ones) = digits vx
        ix = i c8
        ram' = ram c8 // [(ix,hundreds), (ix + 1,tens), (ix + 2, ones)]

    -- step pc. dump v(0)..v(x) in ram at i
    (0xF, _, 0x5, 0x5) -> stepPC . setRAM ram' $ c8
      where
        ram' = copyTo (ram c8,i c8) (v c8,0) (fromIntegral x)

    -- step pc. load v(0)..v(x) from ram at i
    (0xF, _, 0x6, 0x5) -> stepPC . setV v' $ c8
      where
        v' = copyTo (v c8,0) (ram c8,i c8) x

    -- step pc. v(x) = d
    (0xF, _, 0x0, 0x7) -> stepPC . setV v' $ c8
      where
        v' = v c8 // [(x,d c8)]

    -- step pc. d = v(x)
    (0xF, _, 0x1, 0x5) -> stepPC . setD (v c8 ! x) $ c8

    -- step pc. s = v(x)
    (0xF, _, 0x1, 0x8) -> stepPC . setS (v c8 ! x) $ c8

    -- if keydown(v(x)) then skip pc else step pc
    (0xE, _, 0x9, 0xE) -> if inputs c8 ! vx then skipPC c8 else stepPC c8

    -- if keydown(v(x)) then step pc else skip pc
    (0xE, _, 0xA, 0x1) -> if inputs c8 ! vx then stepPC c8 else skipPC c8

    -- if keydown v(x) then step pc else do nothing
    (0xF, _, 0x0, 0xA) -> if inputs c8 ! vx then stepPC c8 else c8

    -- fail hard if encounter unknown nibble-pattern
    _ -> error $ "unknown opcode: " ++ show (a,x,y,z)
    where 
      (a,x,y,z) = fetchNibbles c8
      vx = v c8 ! x
      vy = v c8 ! y
      v0 = v c8 ! 0x0
      nnn = word16FromNibbles x y z
      nn = word8FromNibbles y z
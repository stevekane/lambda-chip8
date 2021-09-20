{-# LANGUAGE RankNTypes #-}

module Chip8Model where

import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector, (!), (//))
import Data.Sequence (Seq, lookup, update)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Control.Lens (Lens', set, over, view, (^.))
import Lib (highNibble, lowNibble)

class TestWank w where
  wank  :: AbstractStack s => Lens' (w (s Word16)) Word16
  jank  :: AbstractStack s => Lens' (w (s Word16)) Word16
  stank :: AbstractStack s => Lens' (w (s Word16)) (s Word16)

-- class AbstractChip8 c where
--   i         :: c -> Word16
--   pc        :: c -> Word16
--   d         :: c -> Word8
--   t         :: c -> Word8
--   stack     :: AbstractStack s => c -> s Word16
--   registers :: AbstractMemory m => c -> m Word8
--   ram       :: AbstractMemory m => c -> m Word8
--   inputs    :: AbstractMemory m => c -> m Bool
--   display   :: Abstract2DField f => c -> f Word32 Bool

-- CHIP8
class AbstractChip8 c where
  i         :: Lens' c Word16
  pc        :: Lens' c Word16
  d         :: Lens' c Word8
  t         :: Lens' c Word8
  stack     :: AbstractStack s => Lens' c (s Word16)
  registers :: Integral i => Lens' c (Buffer i Word16)
  ram       :: Integral i => Lens' c (Buffer i Word8)
  inputs    :: Integral i => Lens' c (Buffer i Bool)
  display   :: Integral i => Lens' c (Buffer i Bool)

  stepPC :: c -> c
  stepPC = over pc (+2)

  skipPC :: c -> c
  skipPC = over pc (+4)

newtype Buffer i a = Buffer { elements :: Lens' i a }

unbuffer :: Buffer i a -> Lens' i a
unbuffer Buffer { elements = e } = e

-- 2D FIELD ( This is just a Lens with the same laws from (i,i) -> a )
-- class Abstract2DField f where
--   get :: (Integral i) => (i,i) -> f i a -> a
--   set :: (Integral i) => (i,i) -> a -> f i a -> f i a
--   -- | Satisfying:
--   -- > get (x,y) . set (x,y) a = a
-- 
-- data Field2D f i a = Field2D i (f a)

-- instance AbstractMemory m => Abstract2DField (Field2D m) where
--   get (x,y) (Field2D w f)   = peek (y * w + x) f
--   set (x,y) a (Field2D w f) = Field2D w (poke (y * w + x) a f)

-- STACK
class AbstractStack s where
  push :: a -> s a -> s a
  pop  :: s a -> (a, s a)
  -- | Satisfying:
  -- > (a, push a s) = pop s

data EEStack i m a = EEStack {
  pointer :: i,
  frames :: m a
}

instance AbstractStack [] where
  push  = (:)
  pop v = (head v,tail v)

instance (Integral i, AbstractMemory m) => AbstractStack (EEStack i m) where
  push a s = s { 
    pointer = pointer s + 1, 
    frames  = poke i a (frames s) 
  } where 
      i = fromIntegral . pointer $ s
  pop s = (a,s')
    where
      i  = fromIntegral . pointer $ s
      a  = peek i (frames s)
      s' = s { pointer = pointer s - 1 }

-- MEMORY ( This is just a Lens with the same laws from i -> a )
class AbstractMemory r where
  peek :: Integral i => i -> r a -> a
  poke :: Integral i => i -> a -> r a -> r a
  -- | Satisfying:
  -- > peek i r . poke i a r = a

instance AbstractMemory Seq where
  peek i r   = fromJust $ lookup (fromIntegral i) r
  poke i a r = update (fromIntegral i) a r

instance AbstractMemory Vector where
  peek i r   = r ! fromIntegral i
  poke i a r = r // [(fromIntegral i,a)]
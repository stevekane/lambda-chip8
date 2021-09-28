{-# LANGUAGE RankNTypes #-}

module Chip8Model where

import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector, (!), (//))
import Data.Sequence (Seq, lookup, update)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Control.Lens (Lens', set, over, view, (^.))
import Lib (highNibble, lowNibble)

-- CHIP8
-- lens law says 
--   view l (set l v s) ≡ v
--   set l (view l s) s ≡ s
--   set l v' (set l v s) ≡ set l v' s
-- stack law says 
  -- pop . push a s ≡ a × s
  --   (fst . pop . push a) s ≡ a
  --   fst . pop . push a ≡ const a
  --   (snd . pop . push a) s ≡ s
  --   snd . pop . push a ≡ id

-- Now the strange thing here is... why is it ok to set / read
-- the values stored in the RAM directly but also restrict
-- the values stored in the stack?
-- How does that actual restriction work?
-- It seems like the interface to the Chip8 is saying "You are allowed
-- to read or write any value to registers or to any RAM bank."
-- You are only allowed to push values onto the stack or pop them off the
-- stack.

-- One could altnernatively argue that the interface to the Chip8 should be
-- the opcodes of the chip8 itself. You could then say "call me with these values"
-- but I do not care what you do internally to implement them so long as it 
-- satisfies the laws of Chip8.

{-
Let's enumerate some laws of Chip8.

CLEAR DISPLAY (00E0)
  clear (i,pc,dt,st,s,v,r,i,d) ≡ (i,pc,dt,st,s,v,r,i,replicate 4096 False)

If we treat chip8 as a function whose domain is the set of names i, pc, dt, etc
then we could create a short-hand for the statement above such as:

  clear c8 ≡ c8 // (d × replicate 4096 False)

RETURN FROM SUBROUTINE (00EE)

  return c8 ≡ c8 // ((pc × fst . pop . stack $ c8) × (stack × snd . pop . stack $ c8))

If we allow ourselves a let-binding sugar we can rewrite the above as:

  return c8 ≡ let r be pop . stack $ c8 in c8 // ((pc × fst r) × (stack × snd r))

And finally, if we allow these bindings to pattern-match then we get this:

  return c8 ≡ let (pc',stack') be pop (stack c8) in ((pc × pc') × (stack × stack'))

What if we try to write these laws more algebraically in terms of the operations only?

  "the stack after return is the same as the stack after popping the stack"

    stack . return ≡ exr . pop . stack 

  "the pc after return is the same as the top after popping the stack"

    pc . return ≡ exl . pop . stack

These two algebraic laws use only morphisms of our Chip8 however they omit an important
detail of the behavior of return: namely what it should do with all the other morphisms.
This turns out to suck really badly as you end up needing to spew nonsense everywhere
to cover all the things return should NOT be doing.

  "return does nothing to the memory"

    r . return ≡ r

  "return does nothing to the delay timer"

    dt . return ≡ dt
  
  "..."

We need some kind of shorthand that is sane that allows us to talk about the results of
transactions including what is unchanged.

Let's pretend for a moment that there are only three functions exposed on the interface.

  -- pc × s × r
  return ≡ (pop . π2) × id

This definition is made possible by the arbitrary proximity of pc and s in the tuple 
of data. The more I think about it, the less I like this reliance on proximity at all.
Instead, I am now thinking of moving away from this style in favor of ONLY using the
fundamental operations provided by the structure.

  -- Any value not mentioned is assumed to be held constant for brevity...

    -- clear display
    pc . clear = stepPC . pc 
    display . clear = replicate 4096 False

    -- return from subroutine
    pc . return = head . pop . stack
    stack . return = tail . pop . stack

    -- set pc to nnn
    pc . setPC = const

    -- call subroutine at nnn
    pc . call = const
    head . pop . stack . call = const pc
-}

class AbstractChip8 c where
  i         :: Lens' c Word16
  pc        :: Lens' c Word16
  d         :: Lens' c Word8
  s         :: Lens' c Word8
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

-- STACK
class AbstractStack s where
  push :: a -> s a -> s a
  pop  :: s a -> (a, s a)
  -- | Satisfying:
  -- > (a, push a s) = pop s
  --   exr (a, push a s) = exr (pop s)
  --   push a = snd . pop

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
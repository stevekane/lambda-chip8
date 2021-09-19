module UnsafeStack where

class UnsafeStack s where
  push :: a -> s a -> s a
  pop  :: s a -> (a,s a)
module Array2D where

import Data.Array (Ix)

class Array2D v where
  width       :: (Ix i, Integral i) => v i e -> i
  height      :: (Ix i, Integral i) => v i e -> i
  at          :: (Ix i, Integral i) => v i e -> (i,i) -> e
  update      :: (Ix i, Integral i) => v i e -> [(i,i,e)] -> v i e
  fill        :: (Ix i, Integral i) => v i e -> e -> v i e
  fill v e = update v [ (x,y,e) | (x,y) <- indices v ]
  indices     :: (Ix i, Integral i) => v i e -> [(i,i)]
  indices v = [ (x,y) | y <- [0..(height v - 1)], x <- [0..(width v - 1)]]
  rowMajor    :: (Ix i, Integral i) => v i e -> [e]
  rowMajor v = [ v `at` (x,y) | y <- [0..(height v - 1)], x <- [0..(width v - 1)]]
  columnMajor :: (Ix i, Integral i) => v i e -> [e]
  columnMajor v = [ v `at` (x,y) | x <- [0..(width v - 1)], y <- [0..(height v - 1)]]
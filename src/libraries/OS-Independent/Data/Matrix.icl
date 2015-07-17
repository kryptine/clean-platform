implementation module Data.Matrix

import StdEnv

instance * (Matrix a) | *, +, zero a where
  (*) m1 m2 = {let m1row = row i m1
               in  {let m2col = col j m2
                    in  mulVec m1row m2col \\ j <- [0..cols m2 - 1]} \\ i <- [0..rows m1 - 1]}

mulVec :: (Vector a) (Vector a) -> a | *, +, zero a
mulVec vl vr
  = sum [vl.[i] * vr.[i] \\ i <- [0..size vl - 1]]

row :: Int (Matrix a) -> Vector a
row n matrix = matrix.[n]

col :: Int (Matrix a) -> Vector a
col n matrix = {row.[n] \\ row <-: matrix}

cols :: (Matrix a) -> Int
cols matrix
  | size matrix > 0 = size matrix.[0]
  | otherwise       = 0

rows :: (Matrix a) -> Int
rows matrix = size matrix

module Util.MathUtils where

import Data.List (nub)
import GHC.Float (floorDouble)

isqrt :: (Integral a) => a -> a
isqrt = floorDouble . sqrt . fromIntegral

factors :: (Integral a) => a -> [a]
factors n = nub . concat $ [[x, q] | x <- [1 .. isqrt n], let (q, r) = divMod n x, r == 0]

module Common where

import           Utils

parse :: String -> [Int]
{-# INLINE parse #-}
parse = map parseInt . lines

fuelForModule :: Int -> Int
{-# INLINE fuelForModule #-}
fuelForModule x = div x 3 - 2

{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

solve :: [(Int, [RGB])] -> Int
solve = sum . map power where
  power (_, (RGB r0 g0 b0):configs) = power' r0 g0 b0 configs

  power' !r !g !b [] = r * g * b
  power' !r !g !b ((RGB r' g' b'):configs) = power' (max r r') (max g g') (max b b') configs

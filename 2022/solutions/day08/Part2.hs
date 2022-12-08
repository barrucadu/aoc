{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 8 parse (show . solve)

solve :: Array Int -> Int
solve arr = maximum $ map lookupScore [(x, y) | x <- [0..maxX], y <- [0..maxY]] where
  maxX = widthArray arr - 1
  maxY = heightArray arr - 1

  lookupScore (x, y) =
    let mine = indexArray arr x y
        up = count mine 0 [(x, y') | y' <- rev (y-1)]
        down = count mine 0 [(x, y') | y' <- [y+1..maxY]]
        left = count mine 0 [(x', y) | x' <- rev (x-1)]
        right = count mine 0 [(x', y) | x' <- [x+1..maxX]]
    in up * down * left * right

  rev (-1) = []
  rev n = n : rev (n-1)

  count _ !acc [] = acc
  count mine !acc ((x, y):xys) =
    let here = indexArray arr x y
    in if here >= mine
       then acc + 1
       else count mine (acc+1) xys

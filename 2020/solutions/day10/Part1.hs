{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

import           Data.List (sort)

import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

parse :: String -> [Int]
parse = sort . map parseInt . lines

solve :: [Int] -> Int
solve = go 0 1 0 where
  go !n1 !n3 !prior (x:xs) =
    let diff = x - prior
    in if | diff == 1 -> go (n1+1) n3 x xs
          | diff == 3 -> go n1 (n3+1) x xs
          | otherwise -> go n1 n3 x xs
  go !n1 !n3 _ [] = n1 * n3

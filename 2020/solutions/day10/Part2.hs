{-# LANGUAGE BangPatterns #-}

import           Data.List (sort)

import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

parse :: String -> [Int]
parse = sort . map parseInt . lines

solve :: [Int] -> Int
solve [] = 0
solve xs0 = solve' (const 0) (const 0) (check 0 1) 0 xs0 where
  solve' _ _ _ out [] = out
  solve' n1 n2 n3 _ (x:xs) =
    let n = n1 x + n2 x + n3 x
    in solve' n2 n3 (check x n) n xs

  check prior v x
    | x - prior <= 3 = v
    | otherwise      = 0

{-# LANGUAGE BangPatterns #-}

import Data.List (transpose)
import Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

parse :: String -> [Bool]
parse = map (go 0 0) . transpose . lines where
  go :: Int -> Int -> String -> Bool
  go !ones !zeroes []
    | ones >= zeroes = True
    | otherwise = False
  go !ones !zeroes ('1':xs) = go (ones+1) zeroes xs
  go !ones !zeroes ('0':xs) = go ones (zeroes+1) xs
  go _ _ _ = error "bad input"

solve :: [Bool] -> Int
solve gammaBits =
  let gamma = bitsToInt gammaBits
      epsilon = bitsToInt (map not gammaBits)
  in gamma * epsilon

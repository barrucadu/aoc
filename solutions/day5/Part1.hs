{-# LANGUAGE BangPatterns #-}

import Data.Char (toUpper)

main :: IO ()
main = do
  input <- init <$> readFile "../inputs/day5.txt"
  print (solve input)

solve :: String -> Int
solve = go 0 [] where
  go !n [] (b:bs) = go (n+1) [b] bs
  go !n _ [] = n
  go !n as0@(a:as) (b:bs)
    | a /= b && toUpper a == toUpper b = go (n-1) as bs
    | otherwise = go (n+1) (b:as0) bs

{-# LANGUAGE BangPatterns #-}

import Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

parse :: String -> [Int]
parse = map parseInt . lines

solve :: [Int] -> Int
solve = countIncrements . makeSums where
  makeSums :: [Int] -> [Int]
  makeSums (a0:b0:cs0) = go (a0+b0) b0 cs0 where
    go sum2 sum1 (a:as) = (sum2 + a) : go (sum1 + a) a as
    go _ _ [] = []
  makeSums _ = error "bad input"

  countIncrements (a0:as) = go 0 a0 as where
    go !n _ [] = n
    go !n a (b:bs)
      | b > a = go (n+1) b bs
      | otherwise = go n b bs
  countIncrements _ = error "bad input"

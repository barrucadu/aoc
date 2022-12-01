{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

parse :: String -> [Int]
parse = map parseInt . lines

solve :: [Int] -> Int
solve (a0:as) = go 0 a0 as where
  go !n _ [] = n
  go !n a (b:bs)
    | b > a = go (n+1) b bs
    | otherwise = go n b bs
solve _ = error "bad input"

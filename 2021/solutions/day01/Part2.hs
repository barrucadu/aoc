{-# LANGUAGE BangPatterns #-}

import Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

parse :: String -> [Int]
parse = map parseInt . lines

solve :: [Int] -> Int
solve (a0:b0:c0:ds0) = go 0 (a0+b0+c0) (b0+c0) c0 ds0 where
  go !n _ _ _ [] = n
  go !n !sum3 !sum2 !sum1 (x:xs) =
    let sum3' = sum2 + x
        sum2' = sum1 + x
        sum1' = x
        n' = if sum3' > sum3 then n + 1 else n
    in go n' sum3' sum2' sum1' xs
solve _ = error "bad input"

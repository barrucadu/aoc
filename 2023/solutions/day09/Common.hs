module Common where

import           Utils

parse :: String -> [[Int]]
parse = map (map go . words) . lines where
  go ('-':ds) = negate (parseInt ds)
  go ds = parseInt ds

extrapolate :: [Int] -> Int
extrapolate (a0:b0:cs0) = let diff = b0 - a0 in go (z diff) [diff] (b0:cs0) where
  z = (==0)

  go allZ diffs (a:b:cs) =
    let diff = b - a
        allZ' = allZ && z diff
    in go allZ' (diff:diffs) (b:cs)
  go True _ [c] = c
  go False diffs [c] = c + extrapolate (reverse diffs)

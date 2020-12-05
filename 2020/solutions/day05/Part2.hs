import Data.List (sort)

import Common
import Utils

main :: IO ()
main = mainFor 5 parse (show . solve)

solve :: [Int] -> Int
solve = go . sort where
  go (s1:ss@(s2:s3:_))
    | s1 + 1 /= s2 = s1 + 1
    | s3 - 1 /= s2 = s3 - 1
    | otherwise = go ss
  go _ = error "no solution"

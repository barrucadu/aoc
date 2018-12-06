import Data.List (foldl')

import Utils

main :: IO ()
main = mainFor 1 (parseInts . lines) (show . solve)

solve :: [Int] -> Int
solve = foldl' (+) 0

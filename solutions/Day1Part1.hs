import Data.List (foldl')

import Utils

main :: IO ()
main = do
  input <- parseInts . lines <$> readFile "../inputs/day1.txt"
  print (solve input)

solve :: [Int] -> Int
solve = foldl' (+) 0

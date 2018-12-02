import Data.List (foldl')

import Utils

main :: IO ()
main = do
  input <- lines <$> readFile "../inputs/day1.txt"
  print (solve input)

solve :: [String] -> Int
solve = foldl' go 0 where
  go acc ('+':n) = acc + parseInt n
  go acc ('-':n) = acc - parseInt n
  go acc _ = acc

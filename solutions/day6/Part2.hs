import Data.List (foldl')

import Common
import Utils

main :: IO ()
main = do
  input <- parse <$> readFile "../inputs/day6.txt"
  print (solve input)

solve :: (Int, Int, Int, Int, [(Int, Int)]) -> Int
solve (xmin, xmax, ymin, ymax, points) = foldl' go 0 [(x, y) | x <- [xmin..xmax], y <- [ymin..ymax]] where
  go total xy =
    if sum (map (manhattan xy) points) < limit
    then total + 1
    else total

  limit = 10000

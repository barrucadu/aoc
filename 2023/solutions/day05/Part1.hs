import           Data.List (foldl')

import           Common
import           Utils

main :: IO ()
main = mainFor 5 parse (show . solve)

solve :: ([Int], [Map]) -> Int
solve (seeds, ms) = minimum [foldl' applyMap s ms | s <- seeds]

applyMap :: Int -> Map -> Int
applyMap x = go where
  go (m:ms)
    | contained m = mrOut m + (x - mrIn m)
    | otherwise = go ms
  go [] = x

  contained m = x >= mrIn m && x < mrEnd m

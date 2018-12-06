{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')

import Utils

main :: IO ()
main = do
  input <- parse <$> readFile "../inputs/day6.txt"
  print (solve input)

parse :: String -> (Int, Int, Int, Int, [(Int, Int)])
parse input0 = (xmin, xmax, ymin, ymax, points) where
  xmin = minimum (map fst points)
  xmax = maximum (map fst points)
  ymin = minimum (map snd points)
  ymax = maximum (map snd points)

  points = map (go 0) (lines input0) where
    go !acc (',':' ':rest) = (acc, go1 0 rest)
    go !acc (d:rest) = go (stepParseInt acc d) rest
    go _ [] = error "invalid input"

    go1 !acc [] = acc
    go1 !acc (d:rest) = go1 (stepParseInt acc d) rest

solve :: (Int, Int, Int, Int, [(Int, Int)]) -> Int
solve (xmin, xmax, ymin, ymax, points) = foldl' go 0 [(x, y) | x <- [xmin..xmax], y <- [ymin..ymax]] where
  go total xy =
    if sum (map (manhattan xy) points) < limit
    then total + 1
    else total

  limit = 10000

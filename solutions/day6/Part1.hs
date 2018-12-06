{-# LANGUAGE BangPatterns #-}

import qualified Data.IntMap.Strict as M
import Data.List (sort)

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
solve (xmin, xmax, ymin, ymax, points) = go initial [(x, y) | x <- [xmin..xmax], y <- [ymin..ymax]] where
  go acc [] = maximum (M.elems acc)
  go acc (xy@(x, y):rest) = case closest xy of
    Just p
      | x == xmin || x == xmax || y == ymin || y == ymax -> go (M.delete (hash p) acc) rest
      | otherwise -> go (M.adjust (+1) (hash p) acc) rest
    Nothing -> go acc rest

  closest xy = case sort [(manhattan p xy, p) | p <- points] of
    ((a, _):(b, _):_) | a == b -> Nothing
    ((_, p):_) -> Just p
    _ -> Nothing

  initial = M.fromList [(hash p, 0) | p <- points]

  hash (x, y) = x * 1000 + y

{-# LANGUAGE BangPatterns #-}

module Common where

import Utils

parse :: String -> (Int, Int, Int, Int, [(Int, Int)])
{-# INLINABLE parse #-}
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

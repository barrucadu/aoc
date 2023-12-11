{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils

type P = (Int, Int)

parse :: String -> [P]
parse = go 0 0 where
  go !_ !_ [] = []
  go !_ !y ('\n':cs) = go 0 (y+1) cs
  go !x !y ('#':cs) = (x, y) : go (x+1) y cs
  go !x !y ('.':cs) = go (x+1) y cs

solveFor :: Int -> [P] -> Int
solveFor factor ps0 = distanceSum $ map expand ps0 where
  distanceSum (xy:xys) = sum (map (manhattan2 xy) xys) + distanceSum xys
  distanceSum [] = 0

  expand (x, y) =
    let xfactor = factor * length (filter (< x) xempty)
        yfactor = factor * length (filter (< y) yempty)
    in (x + xfactor, y + yfactor)

  xempty = filter (`notElem` xs) [xmin..xmax]
  yempty = filter (`notElem` ys) [ymin..ymax]

  (xmin, xmax) = minmax xs
  (ymin, ymax) = minmax ys

  xs = map fst ps0
  ys = map snd ps0

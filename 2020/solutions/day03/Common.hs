{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.Set as S

type TreeMap = (S.Set (Int, Int), Int, Int)

parse :: String -> TreeMap
parse input0 = go S.empty 0 css0 where
  css0@(cs0:_) = lines input0

  go !acc !y (cs:css) = go (go' acc y 0 cs) (y-1) css
  go !acc _ [] = (acc, negate (length css0), negate (length cs0))

  go' !acc !y !x (c:cs) =
    let acc' = if c == '.' then acc else S.insert (x, y) acc
    in go' acc' y (x-1) cs
  go' !acc _ _ [] = acc

slope :: TreeMap -> Int -> Int -> Int
slope (treemap, ymin, xmax) dx dy = go 0 0 0 where
  go !x !y !acc
    | y <= ymin = acc
    | otherwise = go ((x+dx) `mod` xmax) (y+dy) $ if (x, y) `S.member` treemap then acc+1 else acc

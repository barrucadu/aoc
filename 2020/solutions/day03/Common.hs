{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

type TreeMap = (M.IntMap S.IntSet, Int, Int)

parse :: String -> TreeMap
parse input0 = go M.empty 0 css0 where
  css0@(cs0:_) = lines input0

  go !acc !y (cs:css) = go (M.insert y (go' S.empty y 0 cs) acc) (y-1) css
  go !acc _ [] = (acc, negate (length css0), negate (length cs0))

  go' !acc !y !x (c:cs) =
    let acc' = if c == '.' then acc else S.insert x acc
    in go' acc' y (x-1) cs
  go' !acc _ _ [] = acc

slope :: TreeMap -> Int -> Int -> Int
slope (treemap, ymin, xmin) dx dy = go 0 0 0 where
  go !x !y !acc
    | y <= ymin = acc
    | otherwise = go ((x+dx) `mod` xmin) (y+dy) $ if x `S.member` M.findWithDefault S.empty y treemap then acc+1 else acc

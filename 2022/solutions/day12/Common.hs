{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict      as M

-- to help part 2, make edges point in the opposite direction
toGraph :: Int -> M.Map (Int, Int) Char -> G.Gr () Int
toGraph width grid = G.mkGraph lnodes ledges where
  lnodes = [(toNode width xy, ()) | xy <- M.keys grid]
  ledges = concatMap toEdges (M.assocs grid)

  toEdges ((x, y), c) =
    let n = toNode width (x, y)
        adjs = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    in [(toNode width a, n, 1) | a <- adjs, M.member a grid, grid M.! a <= succ c]

toGrid :: String -> (M.Map (Int, Int) Char, (Int, Int), (Int, Int))
toGrid = go M.empty undefined undefined 0 0 . lines where
  go !m start end _ _ [] = (m, start, end)
  go !m start end _ !y ([]:ls) = go m start end 0 (y+1) ls
  go !m start end !x !y ((c:l):ls)
    | c == 'S' = go (M.insert (x, y) 'a' m) (x, y) end (x+1) y (l:ls)
    | c == 'E' = go (M.insert (x, y) 'z' m) start (x, y) (x+1) y (l:ls)
    | otherwise = go (M.insert (x, y) c m) start end (x+1) y (l:ls)

toNode :: Int -> (Int, Int) -> Int
toNode width (x, y) = x + y * width

widthOf :: String -> Int
widthOf = length . head . lines

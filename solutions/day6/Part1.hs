{-# LANGUAGE BangPatterns #-}

import qualified Data.Vector as V

import Common
import Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

solve :: [(Int, Int)] -> Int
solve points0 = search 0 0 points0 where
  search best _ [] = best
  search best !idx (p:rest)
    | edge p = search best (idx+1) rest
    | otherwise = search (flood best idx p) (idx+1) rest

  flood best0 idx p@(px, py) = go 1 1 where
    go !n !delta =
      let xs = [(x, y) | x <- [px-delta..px+delta], y <- [py-delta, py+delta]]
          ys = [(x, y) | x <- [px-delta, px+delta], y <- [py-delta+1..py+delta-1]]
      in case go1 (Just 0) (xs++ys) of
        Just 0 -> max n best0
        Just f -> go (n+f) (delta+1)
        Nothing -> best0

    go1 f@(Just fn) (xy:rest)
      | isClosest idx p xy = if edge xy then Nothing else go1 (Just (fn+1)) rest
      | otherwise = go1 f rest
    go1 f _ = f

  isClosest n0 p0 xy = go (V.length points - 1) where
    go !n
      | n == -1 = True
      | n == n0 = go (n-1)
      | otherwise = dist < manhattan (V.unsafeIndex points n) xy && go (n-1)

    dist = manhattan p0 xy

  edge (x, y) = x == xmin || x == xmax || y == ymin || y == ymax

  (xmin, xmax) = minmax (map fst points0)
  (ymin, ymax) = minmax (map snd points0)

  points = V.fromList points0

{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as M

import Common
import Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

solve :: (Int, Int, Int, Int, [(Int, Int)]) -> Int
solve (xmin, xmax, ymin, ymax, points) = search M.empty points where
  search acc [] = maximum (M.elems acc)
  search acc (p:rest)
    | edge p = search acc rest
    | otherwise = search (flood acc p) rest

  flood acc0 p@(px, py) = go 1 1 where
    go !n !delta =
      let xs = [(x, y) | x <- [px-delta..px+delta], y <- [py-delta, py+delta]]
          ys = [(x, y) | x <- [px-delta, px+delta], y <- [py-delta+1..py+delta-1]]
      in case go1 (Just 0) (xs++ys) of
        Just 0 -> M.insert p n acc0
        Just f -> go (n+f) (delta+1)
        Nothing -> acc0

    go1 f@(Just fn) (xy:rest)
      | isClosest p xy = if edge xy then Nothing else go1 (Just (fn+1)) rest
      | otherwise = go1 f rest
    go1 f _ = f

  isClosest p0 xy = go points where
    go (p:ps)
      | p == p0 = go ps
      | otherwise = (dist < manhattan p xy) && go ps
    go [] = True

    dist = manhattan p0 xy

  edge (x, y) = x == xmin || x == xmax || y == ymin || y == ymax

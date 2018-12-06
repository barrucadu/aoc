{-# LANGUAGE BangPatterns #-}

import Data.List (sort)

import Common
import Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

solve :: (Int, Int, Int, Int, [(Int, Int)]) -> Int
solve (_, _, _, _, points) = go 1 1 where
  go !n !delta =
    let xs = [(x, y) | x <- [px-delta..px+delta], y <- [py-delta, py+delta]]
        ys = [(x, y) | x <- [px-delta, px+delta], y <- [py-delta+1..py+delta-1]]
    in case go1 0 (xs++ys) of
         0 -> n
         f -> go (n+f) (delta+1)

  go1 !f (xy:rest)
    | inRange xy = go1 (f+1) rest
    | otherwise = go1 f rest
  go1 !f _ = f

  px = median (sort (map fst points))
  py = median (sort (map snd points))

  inRange xy = sum (map (manhattan xy) points) < 10000

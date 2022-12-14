{-# LANGUAGE BangPatterns #-}

import qualified Data.IntSet as S

import           Common
import           Utils

main :: IO ()
main = mainFor 14 parse (show . solve)

solve :: [Path] -> Int
solve paths = waterfall points0 where
  (points0, maxY) = toPoints paths

  waterfall :: S.IntSet -> Int
  waterfall = loop 0 where
    loop :: Int -> S.IntSet -> Int
    loop !n !points
      | isSolid (500, 0) points = n
      | otherwise = loop (n+1) (S.insert (pointToInt maxY $ fall points 500 0) points)

    fall :: S.IntSet -> Int -> Int -> Point
    fall points !x !y
      | not $ (x, y+1) `isSolid` points = fall points x (y+1)
      | not $ (x-1, y+1) `isSolid` points = fall points (x-1) (y+1)
      | not $ (x+1, y+1) `isSolid` points = fall points (x+1) (y+1)
      | otherwise = (x, y)

    isSolid :: Point -> S.IntSet -> Bool
    isSolid xy@(_, y) points = y == maxY + 2 || pointToInt maxY xy `S.member` points

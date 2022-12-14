{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as S

import           Common
import           Utils

main :: IO ()
main = mainFor 14 parse (show . solve)

solve :: [Path] -> Int
solve paths = waterfall points0 where
  points0 :: S.Set Point
  points0 = toPoints paths

  maxY :: Int
  maxY = maximum [y | (_, y) <- S.elems points0]

  waterfall :: S.Set Point -> Int
  waterfall = loop 0 where
    loop :: Int -> S.Set Point -> Int
    loop !n !points
      | (500, 0) `S.member` points = n
      | otherwise = loop (n+1) (S.insert (fall points 500 0) points)

    fall :: S.Set Point -> Int -> Int -> Point
    fall points !x !y
      | not $ (x, y+1) `isSolid` points = fall points x (y+1)
      | not $ (x-1, y+1) `isSolid` points = fall points (x-1) (y+1)
      | not $ (x+1, y+1) `isSolid` points = fall points (x+1) (y+1)
      | otherwise = (x, y)

    isSolid :: Point -> S.Set Point -> Bool
    isSolid xy@(_, y) points = y == maxY + 2 || xy `S.member` points

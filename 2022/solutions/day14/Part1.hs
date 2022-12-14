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
    loop !n !points = case fall points 500 0 of
      Just xy -> loop (n+1) (S.insert (pointToInt maxY xy) points)
      Nothing -> n

    fall :: S.IntSet -> Int -> Int -> Maybe Point
    fall points !x !y
      | y > maxY = Nothing
      | not $ (x, y+1) `isSolid` points = fall points x (y+1)
      | not $ (x-1, y+1) `isSolid` points = fall points (x-1) (y+1)
      | not $ (x+1, y+1) `isSolid` points = fall points (x+1) (y+1)
      | otherwise = Just (x, y)

    isSolid :: Point -> S.IntSet -> Bool
    isSolid xy points = pointToInt maxY xy `S.member` points

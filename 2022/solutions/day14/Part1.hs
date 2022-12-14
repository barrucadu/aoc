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
    loop !n !points = case fall points 500 0 of
      Just xy -> loop (n+1) (S.insert xy points)
      Nothing -> n

    fall :: S.Set Point -> Int -> Int -> Maybe Point
    fall points !x !y
      | y > maxY = Nothing
      | (x, y+1) `S.notMember` points = fall points x (y+1)
      | (x-1, y+1) `S.notMember` points = fall points (x-1) (y+1)
      | (x+1, y+1) `S.notMember` points = fall points (x+1) (y+1)
      | otherwise = Just (x, y)

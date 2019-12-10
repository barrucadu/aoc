module Common where

import qualified Data.Map.Strict as M
import           Data.Ratio
import qualified Data.Set        as S

parse :: String -> S.Set (Int, Int)
{-# INLINABLE parse #-}
parse input = S.fromList
  [ (x, y)
  | (y, l) <- zip [0..] (lines input)
  , (x, a) <- zip [0..] l
  , a == '#'
  ]

asteroidsByAngle :: (Int, Int) -> S.Set (Int, Int) -> M.Map Double [(Int, Int)]
{-# INLINABLE asteroidsByAngle #-}
asteroidsByAngle xy0 = go M.empty . S.toList . S.delete xy0 where
  go map_ [] = map_
  go map_ (xy:xys) =
    let theta = angle xy0 xy
    in go (M.alter (addPoint xy) theta map_) xys

  addPoint xy (Just xys) = Just (xy:xys)
  addPoint xy Nothing = Just [xy]

distanceFrom :: (Int, Int) -> (Int, Int) -> Int
{-# INLINABLE distanceFrom #-}
distanceFrom (x0, y0) (x1, y1) = (x1 - x0) ^ 2 + (y1 - y0) ^ 2

angle :: (Int, Int) -> (Int, Int) -> Double
{-# INLINABLE angle #-}
angle (x0, y0) (x1, y1) =
  let dx = fromIntegral (x1 - x0)
      dy = -fromIntegral (y1 - y0)
  in atan2 dy dx

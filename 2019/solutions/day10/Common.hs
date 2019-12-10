module Common where

import           Data.Ratio
import qualified Data.Set   as S

parse :: String -> S.Set (Int, Int)
{-# INLINABLE parse #-}
parse input = S.fromList
  [ (x, y)
  | (y, l) <- zip [0..] (lines input)
  , (x, a) <- zip [0..] l
  , a == '#'
  ]

asteroidsBetween :: S.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
{-# INLINABLE asteroidsBetween #-}
asteroidsBetween asteroids xy0 xy1
    | fst xy0 <= fst xy1 = go xy0 xy1
    | otherwise = go xy1 xy0
  where
    go (x0, y0) (x1, y1)
      | x0 == x1 = [(x0, y) | y <- [min y0 y1..max y0 y1], y /= y0, y /= y1, hasAsteroid x0 y]
      | y0 == y1 = [(x, y0) | x <- [x0..x1], x /= x0, x /= x1, hasAsteroid x y0]
      | otherwise =
        let m = (y0 - y1) % (x0 - x1)
            c = fromIntegral y0 - (m * fromIntegral x0);
        in [ (x, y')
           | x <- [x0..x1]
           , let y = m * fromIntegral x + c
           , denominator y == 1
           , let y' = numerator y
           , (x, y') /= (x0, y0)
           , (x, y') /= (x1, y1)
           , hasAsteroid x y'
           ]

    hasAsteroid x y = S.member (x, y) asteroids

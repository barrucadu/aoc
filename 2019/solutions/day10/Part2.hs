{-# LANGUAGE BangPatterns #-}

import           Data.Foldable   (maximumBy)
import           Data.List       (sortOn)
import qualified Data.Map.Strict as M
import           Data.Ord        (Down(..), comparing)
import qualified Data.Set        as S

import           Common
import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

solve :: S.Set (Int, Int) -> Int
solve asteroids0 = go goal (M.delete xy0 $ M.fromSet (angle xy0) asteroids0) where
  xy0 = startingCoordinates asteroids0

  goal = 200 :: Int

  go !n !asteroids =
    let q1 = map fst $ sortOn (Down . snd) [(xy, theta) | (xy, theta) <- M.toList asteroids, theta > 0, theta <= pi/2]
        q2 = map fst $ sortOn (Down . snd) [(xy, theta) | (xy, theta) <- M.toList asteroids, theta > -pi/2, theta <= 0]
        q3 = map fst $ sortOn (Down . snd) [(xy, theta) | (xy, theta) <- M.toList asteroids, theta <= -pi/2]
        q4 = map fst $ sortOn (Down . snd) [(xy, theta) | (xy, theta) <- M.toList asteroids, theta > pi/2]
    in case blast asteroids n (q1++q2++q3++q4) of
         Right (x, y) -> x * 100 + y
         Left (asteroids', n') -> go n' asteroids'

  blast asteroids = blast' asteroids where
    blast' !asteroids' !n [] = Left (asteroids', n)
    blast' !asteroids' !n (xy:xys)
      | hasLineOfSight (M.keysSet asteroids) xy0 xy = if n == 1 then Right xy else blast' (M.delete xy asteroids') (n-1) xys
      | otherwise = blast' asteroids' n xys

startingCoordinates :: S.Set (Int, Int) -> (Int, Int)
startingCoordinates asteroids = maximumBy (comparing (length . visibleAsteroids)) asteroids where
  visibleAsteroids xy0 = [xy | xy <- S.toList asteroids, null (asteroidsBetween asteroids xy0 xy), xy /= xy0]

hasLineOfSight :: S.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
hasLineOfSight asteroids xy0 xy1 = null (asteroidsBetween asteroids xy0 xy1) && xy0 /= xy1

angle :: (Int, Int) -> (Int, Int) -> Double
angle (x0, y0) (x1, y1) =
  let dx = fromIntegral (x1 - x0)
      dy = -fromIntegral (y1 - y0)
  in atan2 dy dx

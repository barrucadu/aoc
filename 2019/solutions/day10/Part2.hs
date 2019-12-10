{-# LANGUAGE BangPatterns #-}

import           Data.List       (sortOn)
import qualified Data.Map.Strict as M
import           Data.Ord        (Down(..))
import qualified Data.Set        as S

import           Common
import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

solve :: S.Set (Int, Int) -> Int
solve asteroids0 = go goal (sortOn (Down . distanceFrom xy0) <$> angles0) where
  (xy0, angles0) = head . sortOn (Down . M.size . snd) $
    [ (xy, asteroidsByAngle xy asteroids0)
    | xy <- S.toList asteroids0
    ]

  goal = 200 :: Int

  go !n !angles =
    let q1 = sortOn Down [theta | theta <- M.keys angles, theta > 0, theta <= pi/2]
        q2 = sortOn Down [theta | theta <- M.keys angles, theta > -pi/2, theta <= 0]
        q3 = sortOn Down [theta | theta <- M.keys angles, theta <= -pi/2]
        q4 = sortOn Down [theta | theta <- M.keys angles, theta > pi/2]
    in case blast angles n (q1++q2++q3++q4) of
         Right (x, y) -> x * 100 + y
         Left (angles', n') -> go n' angles'

  blast angles = blast' angles where
    blast' !angles' !n [] = Left (angles', n)
    blast' !angles' !n (t:ts) = case M.lookup t angles' of
      Just (xy:xys)
        | n == 1    -> Right xy
        | otherwise -> blast' (M.insert t xys angles') (n-1) ts
      _ -> blast' (M.delete t angles') n ts

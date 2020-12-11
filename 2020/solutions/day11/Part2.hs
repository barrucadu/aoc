{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Maybe (mapMaybe)

import Common
import Utils

main :: IO ()
main = mainFor 11 parse (show . solve)

solve :: M.Map Point State -> Int
solve m0 = genericSolve 5 neighbours m0 where
  ymax = S.findMax . S.map (\(P y _) -> y) $ M.keysSet m0
  xmax = S.findMax . S.map (\(P _ x) -> x) $ M.keysSet m0

  neighbours = M.mapWithKey neighbours' m0

  neighbours' yx _ = mapMaybe (lineOfSightFrom yx) dyxs

  lineOfSightFrom p0 dyx = los (dyx p0) where
    los !p@(P y x)
      | M.member p m0 = Just p
      | y < 0 || y > ymax = Nothing
      | x < 0 || x > xmax = Nothing
      | otherwise = los (dyx p)

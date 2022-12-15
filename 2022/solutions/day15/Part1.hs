{-# LANGUAGE BangPatterns #-}

import           Data.List (foldl', nub)

import           Common
import           Utils

main :: IO ()
main = mainFor 15 parse (show . solve)

solve :: [(P, P)] -> Int
solve positions = overlapSize - length beacons where
  overlapSize :: Int
  overlapSize = fst $ foldl' go (rxmax0 - rxmin0 + 1, rxmax0) rs0 where
    ((rxmin0, rxmax0):rs0) = ranges

    go (!acc, xmax) (rxmin, rxmax)
      | rxmax < xmax = (acc, xmax)
      | rxmin > xmax = (acc + rxmax - rxmin + 1, rxmax)
      | otherwise = (acc + rxmax - xmax, rxmax)

  ranges :: [(Int, Int)]
  ranges = toRanges targetY positions

  beacons :: [Int]
  beacons = nub [bx | (_, (bx, by)) <- positions, by == targetY]

  targetY :: Int
  targetY = 2000000

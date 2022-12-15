{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 15 parse (show . solve)

solve :: [(P, P)] -> Int
solve positions = go [minY..maxY] where
  go :: [Int] -> Int
  go (y:ys) = case findGap rxmax0 rs0 of
      Just x -> x * 4000000 + y
      Nothing -> go ys
    where
      ((_, rxmax0):rs0) = toRanges y positions

      findGap xmax ((rxmin, rxmax):rs)
        | rxmax > maxX = Nothing
        | rxmin == xmax + 2 = Just (xmax+1)
        | otherwise = findGap (max xmax rxmax) rs
      findGap _ [] = Nothing

  maxX :: Int
  maxX = 4000000

  minY :: Int
  minY = 0

  maxY :: Int
  maxY = 4000000

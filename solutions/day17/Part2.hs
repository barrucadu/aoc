{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 17 parse (show . solve)

solve :: [Range] -> Int
solve ranges = go 0 minX minY where
  go !acc !x !y
    | x == maxX + 1 = go acc minX (y+1)
    | y == maxY + 1 = acc
    | otherwise = case indexArray arr x y of
        SWaterStanding -> go (acc+1) (x+1) y
        _ -> go acc (x+1) y

  (minX, maxX, minY, maxY, arr) = waterfall ranges

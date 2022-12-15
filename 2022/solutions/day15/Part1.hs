{-# LANGUAGE BangPatterns #-}

import           Data.List  (foldl', nub, sort)
import           Data.Maybe (mapMaybe)

import           Utils

main :: IO ()
main = mainFor 15 parse (show . solve)

type P = (Int, Int)

parse :: String -> [(P, P)]
parse = map go . lines where
  go (_:_:_:_:_:_:_:_:_:_:l) =
    let (sxy, _:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:l') = parseXY l
        (bxy, []) = parseXY l'
    in (sxy, bxy)

  parseXY (_:_:l) =
    let (x, _:_:_:l') = parseNum False 0 l
        (y, l'') = parseNum False 0 l'
    in ((x, y), l'')

  parseNum neg !acc [] = (if neg then negate acc else acc, [])
  parseNum neg !acc (',':rest) = (if neg then negate acc else acc, rest)
  parseNum neg !acc (':':rest) = (if neg then negate acc else acc, rest)
  parseNum _ !acc ('-':rest) = parseNum True acc rest
  parseNum neg !acc (x:xs) = parseNum neg (stepParseInt acc x) xs

solve :: [(P, P)] -> Int
solve positions = overlapSize - length beacons where
  overlapSize :: Int
  overlapSize = fst $ foldl' go (rxmax0 - rxmin0 + 1, rxmax0) rs0 where
    ((rxmin0, rxmax0):rs0) = sort ranges

    go (!acc, xmax) (rxmin, rxmax)
      | rxmax < xmax = (acc, xmax)
      | rxmin > xmax = (acc + rxmax - rxmin + 1, rxmax)
      | otherwise = (acc + rxmax - xmax, rxmax)

  ranges :: [(Int, Int)]
  ranges = mapMaybe go positions where
    go (sxy@(sx, sy), bxy) =
      let srange = manhattan2 sxy bxy
          ydist = abs (sy - targetY)
          xdist = srange - ydist
      in if xdist < 0 then Nothing else Just (sx - xdist, sx + xdist)

  beacons :: [Int]
  beacons = nub [bx | (_, (bx, by)) <- positions, by == targetY]

  targetY :: Int
  targetY = 2000000

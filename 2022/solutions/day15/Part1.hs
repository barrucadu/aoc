{-# LANGUAGE BangPatterns #-}

import qualified Data.IntSet as S

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
solve positions = S.size $ S.unions (map overlap positions) `S.difference` beacons where
  beacons :: S.IntSet
  beacons = S.fromList [bx | (_, (bx, by)) <- positions, by == targetY]

  overlap :: (P, P) -> S.IntSet
  overlap (sxy@(sx, sy), bxy) =
    let srange = manhattan2 sxy bxy
        ydist = abs (sy - targetY)
        xdists = [0..srange - ydist]
    in S.fromList [sx + dx * m | m <- [-1, 1], dx <- xdists]

  targetY = 2000000

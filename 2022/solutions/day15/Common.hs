{-# LANGUAGE BangPatterns #-}

module Common where

import           Data.List  (sort)
import           Data.Maybe (mapMaybe)

import           Utils      (manhattan2, stepParseInt)

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

toRanges :: Int -> [(P, P)] -> [(Int, Int)]
toRanges targetY = sort . mapMaybe go where
  go :: (P, P) -> Maybe (Int, Int)
  go (sxy@(sx, sy), bxy) =
    let srange = manhattan2 sxy bxy
        ydist = abs (sy - targetY)
        xdist = srange - ydist
    in if xdist < 0 then Nothing else Just (sx - xdist, sx + xdist)

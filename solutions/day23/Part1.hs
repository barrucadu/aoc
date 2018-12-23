{-# LANGUAGE BangPatterns #-}

import Data.List (maximumBy)
import Data.Ord (comparing)

import Utils

main :: IO ()
main = mainFor 23 parse (show . solve)

parse :: String -> [((Int, Int, Int), Int)]
parse = map go . lines where
  go ('p':'o':'s':'=':'<':rest) =
    let (x, rest') = goI rest
        (y, rest'') = goI rest'
        (z, ',':' ':'r':'=':rest''') = goI rest''
        (r, _) = goI rest'''
    in ((x, y, z), r)
  go _ = error "invalid input"

  goI ('-':rest) = let (i, r) = goI' 0 rest in (negate i, r)
  goI rest = goI' 0 rest

  goI' !acc (',':rest) = (acc, rest)
  goI' !acc ('>':rest) = (acc, rest)
  goI' !acc (d:rest)   = goI' (stepParseInt acc d) rest
  goI' !acc [] = (acc, [])

solve :: [((Int, Int, Int), Int)] -> Int
solve input0 = length (filter inRange input0) where
  (xy0, r0) = maximumBy (comparing snd) input0

  inRange (xy, _) = manhattan3 xy0 xy <= r0

{-# LANGUAGE BangPatterns #-}

import           Data.List (minimum)
import qualified Data.Set  as S

import           Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

parse :: String -> (S.Set (Int, Int), S.Set (Int, Int))
parse = go . lines where
  go (wire1:wire2:_) = (goW S.empty 0 0 wire1, goW S.empty 0 0 wire2)
  go _ = error "bad input"

  goW !set !x !y ('R':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [(x + d, y) | d <- [0..range]])
    in goW set' (x + range) y rest'
  goW !set !x !y ('L':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [(x - d, y) | d <- [0..range]])
    in goW set' (x - range) y rest'
  goW !set !x !y ('U':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [(x, y - d) | d <- [0..range]])
    in goW set' x (y - range) rest'
  goW !set !x !y ('D':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [(x, y + d) | d <- [0..range]])
    in goW set' x (y + range) rest'
  goW !set _ _ _ = set

  goI !acc (',':rest) = (acc, rest)
  goI !acc (c:rest) = goI (stepParseInt acc c) rest
  goI !acc [] = (acc, [])

solve :: (S.Set (Int, Int), S.Set (Int, Int)) -> Int
solve (wire1, wire2) = minimum . map (manhattan2 origin)  $ S.toList crossings where
  crossings = S.intersection wire1' wire2'
  wire1' = S.delete origin wire1
  wire2' = S.delete origin wire2

  origin = (0, 0)

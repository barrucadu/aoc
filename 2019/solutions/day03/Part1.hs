{-# LANGUAGE BangPatterns #-}

import           Data.List (minimum)
import qualified Data.Set  as S

import           Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

parse :: String -> S.Set (Int, Int)
parse = go . lines where
  go (wire1:wire2:_) =
    let wire1Path = goW (const True) S.empty 0 0 wire1
    in goW (`S.member` wire1Path) S.empty 0 0 wire2
  go _ = error "bad input"

  goW p !set !x !y ('R':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [point | d <- [0..range], let point = (x + d, y), p point])
    in goW p set' (x + range) y rest'
  goW p !set !x !y ('L':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [point | d <- [0..range], let point = (x - d, y), p point])
    in goW p set' (x - range) y rest'
  goW p !set !x !y ('U':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [point | d <- [0..range], let point = (x, y - d), p point])
    in goW p set' x (y - range) rest'
  goW p !set !x !y ('D':rest) =
    let (range, rest') = goI 0 rest
        set' = S.union set (S.fromList [point | d <- [0..range], let point = (x, y + d), p point])
    in goW p set' x (y + range) rest'
  goW _ !set _ _ _ = set

  goI !acc (',':rest) = (acc, rest)
  goI !acc (c:rest) = goI (stepParseInt acc c) rest
  goI !acc [] = (acc, [])

solve :: S.Set (Int, Int) -> Int
solve = minimum . map (manhattan2 origin) . S.toList . S.delete origin where
  origin = (0, 0)

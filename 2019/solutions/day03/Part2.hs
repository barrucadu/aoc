{-# LANGUAGE BangPatterns #-}

import           Data.List       (minimum)
import qualified Data.Map.Strict as M
import           Data.Maybe      (maybeToList)

import           Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

parse :: String -> M.Map (Int, Int) Int
parse = go . lines where
  go (wire1:wire2:_) =
    let wire1Path = goW (const (Just 0)) M.empty 0 0 0 wire1
    in goW (`M.lookup` wire1Path) M.empty 0 0 0 wire2
  go _ = error "bad input"

  goW f !map_ !x !y !steps ('R':rest) =
    let (range, rest', map_') = goW' (\d -> (x + d, y)) f map_ steps rest
    in goW f map_' (x + range) y (steps + range) rest'
  goW f !map_ !x !y !steps ('L':rest) =
    let (range, rest', map_') = goW' (\d -> (x - d, y)) f map_ steps rest
    in goW f map_' (x - range) y (steps + range) rest'
  goW f !map_ !x !y !steps ('U':rest) =
    let (range, rest', map_') = goW' (\d -> (x, y - d)) f map_ steps rest
    in goW f map_' x (y - range) (steps + range) rest'
  goW f !map_ !x !y !steps ('D':rest) =
    let (range, rest', map_') = goW' (\d -> (x, y + d)) f map_ steps rest
    in goW f map_' x (y + range) (steps + range) rest'
  goW _ !map_ _ _ _ _ = map_

  {-# INLINE goW' #-}
  goW' pf f !map_ !steps rest =
    let (range, rest') = goI 0 rest
        map_' = M.union map_ (M.fromList [(point, steps + d + cost) | d <- [0..range], let point = pf d, cost <- maybeToList $ f point])
    in (range, rest', map_')

  goI !acc (',':rest) = (acc, rest)
  goI !acc (c:rest) = goI (stepParseInt acc c) rest
  goI !acc [] = (acc, [])

solve :: M.Map (Int, Int) Int -> Int
solve = minimum . M.elems . M.delete origin where
  origin = (0, 0)

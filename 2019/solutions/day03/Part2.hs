{-# LANGUAGE BangPatterns #-}

import           Data.List       (sort)
import qualified Data.Map.Strict as M

import           Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

parse :: String -> (M.Map (Int, Int) Int, M.Map (Int, Int) Int)
parse = go . lines where
  go (wire1:wire2:_) = (goW M.empty 0 0 0 wire1, goW M.empty 0 0 0 wire2)
  go _ = error "bad input"

  goW !map_ !x !y !steps ('R':rest) =
    let (range, rest') = goI 0 rest
        map_' = M.union map_ (M.fromList [((x + d, y), steps + d) | d <- [0..range]])
    in goW map_' (x + range) y (steps + range) rest'
  goW !map_ !x !y !steps ('L':rest) =
    let (range, rest') = goI 0 rest
        map_' = M.union map_ (M.fromList [((x - d, y), steps + d) | d <- [0..range]])
    in goW map_' (x - range) y (steps + range) rest'
  goW !map_ !x !y !steps ('U':rest) =
    let (range, rest') = goI 0 rest
        map_' = M.union map_ (M.fromList [((x, y - d), steps + d) | d <- [0..range]])
    in goW map_' x (y - range) (steps + range) rest'
  goW !map_ !x !y !steps ('D':rest) =
    let (range, rest') = goI 0 rest
        map_' = M.union map_ (M.fromList [((x, y + d), steps + d) | d <- [0..range]])
    in goW map_' x (y + range) (steps + range) rest'
  goW !map_ _ _ _ _ = map_

  goI !acc (',':rest) = (acc, rest)
  goI !acc (c:rest) = goI (stepParseInt acc c) rest
  goI !acc [] = (acc, [])

solve :: (M.Map (Int, Int) Int, M.Map (Int, Int) Int) -> Int
solve (wire1, wire2) = head . sort $ M.elems crossings where
  crossings = M.intersectionWith (+) wire1' wire2'
  wire1' = M.delete origin wire1
  wire2' = M.delete origin wire2

  origin = (0, 0)

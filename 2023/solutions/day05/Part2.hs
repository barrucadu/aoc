import           Data.List (foldl')

import           Common
import           Utils

main :: IO ()
main = mainFor 5 parse (show . solve)

solve :: ([Int], [Map]) -> Int
solve (seeds, ms) = minimum . map fst $ foldl' mapRanges ranges ms where
  mapRanges :: [Range] -> Map -> [Range]
  mapRanges rs m = concatMap (\r -> mapRange r m) rs

  ranges :: [Range]
  ranges = go seeds where
    go (s:l:xs) = (s, s + l - 1) : go xs
    go [] = []

type Range = (Int, Int)

mapRange :: Range -> Map -> [Range]
mapRange r0 [] = [r0]
mapRange r0 (m:ms) = concatMap go (split r0 m) where
  go r
    | contained r = [transform r]
    | otherwise = mapRange r ms

  contained (from, to) = from >= mrIn m && to < mrEnd m
  transform (from, to) = (mrOut m + from - mrIn m, mrOut m + to - mrIn m)

split :: Range -> MapRange -> [Range]
split r@(from, to) m
  -- r is after m
  | from > mrEnd m = [r]
  -- r is before m
  | to < mrIn m = [r]
  -- r starts before m and ends after m (m is entirely contained in r)
  | from < mrIn m && to >= mrEnd m = [(from, mrIn m - 1), (mrIn m, mrEnd m - 1), (mrEnd m, to)]
  -- r starts before m and ends inside m
  | from < mrIn m && to < mrEnd m = [(from, mrIn m - 1), (mrIn m, to)]
  -- r starts inside m and ends after m
  | to >= mrEnd m = [(from, mrEnd m - 1), (mrEnd m, to)]
  -- r starts inside m and ends inside m (r is entirely contained in m)
  | otherwise = [r]

module Utils where

import Data.Char (ord)
import Data.List (foldl')

{- | Converts a number to an integer.  Assumes no leading "+" or "-".

Using 'ord' here rather than 'digitToInt' is slightly faster for day 1
part 1.  'read' handles scientific notation and the like, so it's also
slower.

With 'digitToInt':

@
    benchmarking ./Day1Part1
    time                 2.759 ms   (2.749 ms .. 2.770 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 2.755 ms   (2.748 ms .. 2.762 ms)
    std dev              24.23 μs   (19.88 μs .. 32.03 μs)
@

With 'ord':

@
    benchmarking ./Day1Part1
    time                 2.756 ms   (2.746 ms .. 2.767 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 2.743 ms   (2.736 ms .. 2.748 ms)
    std dev              18.28 μs   (13.35 μs .. 27.72 μs)
@
-}
parseInt :: String -> Int
parseInt = foldl' go 0 where
  go acc c = acc * 10 + ord c - ord '0'

-- | Convert a list of strings to ints, assuming each one is in the
-- format "+\d+" or "-\d+".
parseInts :: [String] -> [Int]
parseInts = map go where
  go ('+':n) = parseInt n
  go ('-':n) = -1 * parseInt n
  go _ = error "expected +\\d+ or -\\d+"

{-# LANGUAGE BangPatterns #-}

module Utils where

import Data.Char (chr, ord)
import Data.List (foldl')

-- | Common main function
mainFor :: Int -> (String -> a) -> (a -> String) -> IO ()
{-# INLINE mainFor #-}
mainFor dayN parse solve = do
  let n = if dayN < 10 then '0' : show dayN else show dayN
  input <- parse <$> readFile ("../inputs/day" ++ n ++ ".txt")
  putStrLn (solve input)

-- | Manhattan distance between two points
manhattan :: (Int, Int) -> (Int, Int) -> Int
{-# INLINE manhattan #-}
manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Min/max from a nonempty list, in pass.
minmax :: Ord a => [a] -> (a, a)
minmax (x0:xs) = foldr (\x (!mi, !ma) -> (min x mi, max x ma)) (x0, x0) xs
minmax _ = error "empty list"

-- | Median of a sorted nonempty list.
median :: [a] -> a
{-# INLINE median #-}
median xs = xs !! pos where
  pos = length xs `div` 2

-- | Lowercase a char.  Assumes it's an ASCII letter.
lowercase :: Char -> Char
{-# INLINE lowercase #-}
lowercase c =
  let diff = ord 'a' - ord 'A'
      o = ord c
  in if o > ord 'Z'
     then c
     else chr (o + diff)

-- | Converts a number to an integer.  Assumes no leading "+" or "-".
parseInt :: String -> Int
{-# INLINE parseInt #-}
parseInt = foldl' stepParseInt 0

{- | Converts a char to a digit.

Using 'ord' here rather than 'digitToInt' is slightly faster for day 1
part 1.

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
stepParseInt :: Int -> Char -> Int
{-# INLINE stepParseInt #-}
stepParseInt acc c = acc * 10 + ord c - ord '0'

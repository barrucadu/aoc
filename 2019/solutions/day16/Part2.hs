{-# LANGUAGE BangPatterns #-}

import           Data.Char (intToDigit)

import           Common
import           Utils

main :: IO ()
main = mainFor 16 parse (map intToDigit . solve)

solve :: [Int] -> [Int]
solve digits0 = take 8 solution where
  solution
    -- At sufficiently high indices, the pattern is just a bunch of
    -- zeroes followed by a bunch of ones, so we can just throw away a
    -- big chunk of the input and do addition.
    | offset >= length longInput `div` 2 = reverse . fastFFT 100 . reverse $ drop offset longInput
    | otherwise = drop offset . fftN 100 $ longInput

  offset =
    let (d1:d2:d3:d4:d5:d6:d7:_) = longInput
    in d7 + d6 * 10 + d5 * 100 + d4 * 1000 + d3 * 10000 + d2 * 100000 + d1 * 1000000

  fastFFT n digits
    | n == 0 = digits
    | otherwise = fastFFT (n-1) (fastStep 0 digits)

  fastStep !acc (d:ds) = let acc' = (d + acc) `rem` 10 in acc' : fastStep acc' ds
  fastStep _ [] = []

  longInput = concat (replicate 10000 digits0)

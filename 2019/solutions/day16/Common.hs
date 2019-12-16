module Common where

import           Utils

parse :: String -> [Int]
{-# INLINABLE parse #-}
parse = go where
  go [] = []
  go ['\n'] = []
  go (d:ds) = parseDigit d : go ds

fftN :: Int -> [Int] -> [Int]
{-# INLINABLE fftN #-}
fftN lim digits0 = fft 0 digits0 where
  fft n digits
    | n == lim = digits
    | otherwise = fft (n+1) (step digits)

  step digits = map (\pat -> abs (sum (zipWith (*) digits pat) `rem` 10)) patterns

  patterns =
    [ drop 1 . cycle $ replicate i 0 ++ replicate i 1 ++ replicate i 0 ++ replicate i (negate 1)
    | i <- zipWith const [1..] digits0
    ]

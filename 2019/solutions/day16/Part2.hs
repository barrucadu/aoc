{-# LANGUAGE BangPatterns #-}

import           Control.Monad.ST            (runST)
import           Data.Char                   (intToDigit)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

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
    | offset >= longInputLen `div` 2 = fastFFT 100 $ drop offset longInput
    | otherwise = drop offset . fftN 100 $ longInput

  offset =
    let (d1:d2:d3:d4:d5:d6:d7:_) = longInput
    in d7 + d6 * 10 + d5 * 100 + d4 * 1000 + d3 * 10000 + d2 * 100000 + d1 * 1000000

  longInput    = concat (replicate repetitions digits0)
  longInputLen = length digits0 * repetitions
  repetitions  = 10000

fastFFT :: Int -> [Int] -> [Int]
{-# INLINABLE fastFFT #-}
fastFFT lim digits0 = runST $ fastFFT' =<< VU.thaw (VU.fromList digits0) where
  fastFFT' vec = fgo 0 where
    fgo n
      | n == lim = VU.toList <$> VU.freeze vec
      | otherwise = step >> fgo (n+1)

    step = sgo 0 (VUM.length vec - 1) where
      sgo !acc i
        | i == -1 = pure ()
        | otherwise = do
            here <- VUM.read vec i
            let acc' = (here + acc) `rem` 10
            VUM.write vec i acc'
            sgo acc' (i-1)

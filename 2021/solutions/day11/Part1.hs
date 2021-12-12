{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST (ST, runST)

import Common
import Utils

main :: IO ()
main = mainFor 11 id (show . solve)

solve :: String -> Int
solve input = runST $ do
    grid <- parseM input
    run grid 100
  where
    run :: STArray s Int -> Int -> ST s Int
    run arr = go 0 where
      go !flashes 0 = pure flashes
      go !flashes n = step arr >>= \flashed -> go (flashes + flashed) (n-1)

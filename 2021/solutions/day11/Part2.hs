{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST (ST, runST)

import Common
import Utils

main :: IO ()
main = mainFor 11 id (show . solve)

solve :: String -> Int
solve input = runST $ do
    grid <- parseM input
    run grid
  where
    run grid = go 1 where
      go !n = step grid >>= \flashed -> if flashed == octopodes then pure n else go (n+1)

      octopodes = widthArray' grid * heightArray' grid

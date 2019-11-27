{-# LANGUAGE BangPatterns #-}

module Common where

import           Control.Monad.ST (ST, runST)
import           Data.Foldable    (for_)

import           Utils

solve :: [Int] -> Int -> (Int, Int, Int)
solve sizes serial = runST (findSlidingWindow =<< summedArray) where
  summedArray :: ST s (STArray s Int)
  summedArray = do
    -- use dimension+1 so we can index from 1..dimension, like the puzzle
    v <- newArray (dimension + 1) (dimension + 1)
    writeArray v 1 1 (power 1 1)
    for_ [2..dimension] $ \x -> do
      s1 <- readArray v (x - 1) 1
      writeArray v x 1 (power x 1 + s1)
    for_ [2..dimension] $ \y -> do
      s1 <- readArray v 1 (y - 1)
      writeArray v 1 y (power 1 y + s1)
    for_ [2..dimension] $ \x ->
      for_ [2..dimension] $ \y -> do
        s1 <- readArray v x (y - 1)
        s2 <- readArray v (x - 1) y
        s3 <- readArray v (x - 1) (y - 1)
        writeArray v x y (power x y + s1 + s2 - s3)
    pure v

  findSlidingWindow :: STArray s Int -> ST s (Int, Int, Int)
  findSlidingWindow v = go (0, 0, 0) 0 sizes where
    go (_, bestx, besty) bestsize [] = pure (bestx, besty, bestsize)
    go b@(best, _, _) bestsize (w:ws) = do
      b'@(best', _, _) <- slidingWindow w v
      if best >= best'
        then go b bestsize ws
        else go b' w ws

  slidingWindow :: Int -> STArray s Int -> ST s (Int, Int, Int)
  slidingWindow window v = go 0 0 0 1 1 where
    go !best !bestx !besty !x !y
      | x == threshold =
          if y == threshold
          then pure (best, bestx + 1, besty + 1)
          else go best bestx besty 1 (y+1)
      | otherwise = do
          best' <- rectSum x y
          if best >= best'
            then go best bestx besty (x+1) y
            else go best' x y (x+1) y

    threshold = dimension - window

    rectSum x y = do
      a <- readArray v x y
      b <- readArray v (x + window) y
      c <- readArray v x (y + window)
      d <- readArray v (x + window) (y + window)
      pure (d + a - b - c)

  power x y =
    let r = x + 10
        p = (r * y + serial) * r
    in (p `div` 100 `mod` 10) - 5

  dimension = 300

{-# LANGUAGE BangPatterns #-}

module Common where

import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)

import qualified Data.Vector.Unboxed.Mutable as V

solve :: [Int] -> Int -> (Int, Int, Int)
solve sizes serial = runST (findSlidingWindow =<< summedArray) where
  summedArray :: ST s (V.STVector s Int)
  summedArray = do
    v <- V.new (dimension * dimension)
    V.unsafeWrite v (at 0 0) (power 1 1)
    for_ [x | x <- [1..dimension-1]] $ \x -> do
      s1 <- V.unsafeRead v (at (x - 1) 0)
      V.unsafeWrite v (at x 0) (power (x+1) 1 + s1)
    for_ [y | y <- [1..dimension-1]] $ \y -> do
      s1 <- V.unsafeRead v (at 0 (y - 1))
      V.unsafeWrite v (at 0 y) (power 1 (y+1) + s1)
    for_ [x | x <- [1..dimension-1]] $ \x ->
      for_ [y | y <- [1..dimension-1]] $ \y -> do
        s1 <- V.unsafeRead v (at x (y - 1))
        s2 <- V.unsafeRead v (at (x - 1) y)
        s3 <- V.unsafeRead v (at (x - 1) (y - 1))
        V.unsafeWrite v (at x y) (power (x+1) (y+1) + s1 + s2 - s3)
    pure v

  findSlidingWindow :: V.STVector s Int -> ST s (Int, Int, Int)
  findSlidingWindow v = go (0, 0, 0) 0 sizes where
    go (_, bestx, besty) bestsize [] = pure (bestx, besty, bestsize)
    go b@(best, _, _) bestsize (w:ws) = do
      b'@(best', _, _) <- slidingWindow w v
      if best >= best'
        then go b bestsize ws
        else go b' w ws

  slidingWindow :: Int -> V.STVector s Int -> ST s (Int, Int, Int)
  slidingWindow window v = go 0 0 0 0 0 where
    go !best !bestx !besty !x !y
      | x == threshold =
          if y == threshold
          then pure (best, bestx + 2, besty + 2)
          else go best bestx besty 0 (y+1)
      | otherwise = do
          best' <- rectSum x y
          if best >= best'
            then go best bestx besty (x+1) y
            else go best' x y (x+1) y

    threshold = dimension - window - 1

    rectSum x y = do
      a <- V.read v (at x y)
      b <- V.read v (at (x + window) y)
      c <- V.read v (at x (y + window))
      d <- V.read v (at (x + window) (y + window))
      pure (d + a - b - c)

  power x y =
    let r = x + 10
        p = (r * y + serial) * r
    in (p `div` 100 `mod` 10) - 5

  at x y = x + y * dimension

  dimension = 300

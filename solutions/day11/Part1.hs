{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)

import qualified Data.Vector.Unboxed.Mutable as V

import Utils

main :: IO ()
main = mainFor 11 (parseInt . init) (show . solve)

solve :: Int -> (Int, Int)
solve serial = runST (slidingWindow =<< summedArray) where
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

  slidingWindow :: V.STVector s Int -> ST s (Int, Int)
  slidingWindow v = go 0 0 0 0 0 where
    go !_ !bestx !besty !296 !296 = pure (bestx + window - 1, besty + window - 1)
    go !best !bestx !besty !296 !y = go best bestx besty 0 (y+1)
    go !best !bestx !besty !x !y = do
      best' <- rectSum v x y
      if best >= best'
        then go best bestx besty (x+1) y
        else go best' x y (x+1) y

  rectSum v x y = do
    a <- V.unsafeRead v (at x y)
    b <- V.unsafeRead v (at (x + window) y)
    c <- V.unsafeRead v (at x (y + window))
    d <- V.unsafeRead v (at (x + window) (y + window))
    pure (d + a - b - c)

  power x y =
    let r = x + 10
        p = (r * y + serial) * r
    in (p `div` 100 `mod` 10) - 5

  at x y = x + y * dimension

  dimension = 300
  window = 3

{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)

import Common
import Utils

main :: IO ()
main = mainFor 22 parse (show . solve)

solve :: (Int, (Int, Int)) -> Int
solve (depth, (tX, tY)) = runST $ do
    gIs <- newArray (tX+1) (tY+1)
    eLs <- newArray (tX+1) (tY+1)

    setGIEL gIs eLs 0  0  0
    for_ [1..tX] $ \x -> setGIEL gIs eLs x 0 (x * 16807)
    for_ [1..tY] $ \y -> setGIEL gIs eLs 0 y (y * 48271)
    for_ [1..tX] $ \x ->
      for_ [1..tY] $ \y -> calcGIEL gIs eLs x y
    setGIEL gIs eLs tX tY 0

    riskLevel eLs
  where
    setGIEL :: STArray s Int -> STArray s Int -> Int -> Int -> Int -> ST s ()
    setGIEL gIs eLs x y gi = do
      let el = (gi + depth) `mod` 20183
      writeArray gIs x y gi
      writeArray eLs x y el

    calcGIEL :: STArray s Int -> STArray s Int -> Int -> Int -> ST s ()
    calcGIEL gIs eLs x y = do
      left <- readArray eLs (x-1) y
      above <- readArray eLs x (y-1)
      let gi = left * above
      setGIEL gIs eLs x y gi

    riskLevel :: STArray s Int -> ST s Int
    riskLevel eLs = go 0 0 0 where
      go !acc x y
        | x > tX = go acc 0 (y+1)
        | y > tY = pure acc
        | otherwise = do
            el <- readArray eLs x y
            go (acc + (el `mod` 3)) (x+1) y

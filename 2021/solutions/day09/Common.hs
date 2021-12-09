{-# LANGUAGE BangPatterns #-}

module Common where

import Control.Monad.ST (ST)
import Data.Foldable (for_)

import Utils

parseM :: String -> ST s (STArray s Int)
parseM input = do
  let ls = lines input
  let width = length (head ls)
  let height = length ls
  arr <- newArray width height
  for_ (zip [0..] ls) $ \(y, l) ->
    for_ (zip [0..] l) $ \(x, c) ->
      writeArray arr x y (parseDigit c)
  pure arr

findMinima :: STArray s Int -> ST s [(Int, Int, Int)]
findMinima arr = go [] 0 0 where
  go out !x !y
    | x == maxX = go out 0 (y+1)
    | y == maxY = pure out
    | otherwise = do
        this <- readArray arr x y
        up <- if y == 0 then pure 999 else readArray arr x (y-1)
        down <- if y == maxY - 1 then pure 999 else readArray arr x (y+1)
        left <- if x == 0 then pure 999 else readArray arr (x-1) y
        right <- if x == maxX - 1 then pure 999 else readArray arr (x+1) y

        let out' = if this < up && this < down && this < left && this < right
                   then (this, x, y):out
                   else out

        go out' (x+1) y

  maxX = widthArray' arr
  maxY = heightArray' arr

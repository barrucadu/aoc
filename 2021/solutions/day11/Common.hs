{-# LANGUAGE BangPatterns #-}

module Common where

import Control.Monad.ST (ST)
import Data.Foldable (for_)
import qualified Data.Set as S

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

step :: STArray s Int -> ST s Int
step arr = do
    increment
    flashed <- flash S.empty 0 0
    for_ (S.toList flashed) $ \(x, y) -> writeArray arr x y 0
    pure (S.size flashed)
  where
    increment =
      for_ [0..maxY-1] $ \y ->
        for_ [0..maxX-1] $ \x ->
          increment' x y

    increment' x y = do
      this <- readArray arr x y
      writeArray arr x y (this+1)

    flash flashed !x !y
      | x == maxX = flash flashed 0 (y+1)
      | y == maxY = pure flashed
      | otherwise = do
          flashed' <- flash' flashed x y
          flash flashed' (x+1) y

    flash' flashed !x !y = do
      this <- readArray arr x y
      if this > 9 && (x, y) `S.notMember` flashed
        then propagateFlash (octopodes x y) (S.insert (x, y) flashed)
        else pure flashed

    propagateFlash [] flashed = pure flashed
    propagateFlash ((x, y):xys) flashed = do
      increment' x y
      flashed' <- flash' flashed x y
      propagateFlash xys flashed'

    octopodes x y =
      [ (x + dx, y + dy)
      | dx <- [-1, 0, 1]
      , x + dx >= 0
      , x + dx < maxX
      , dy <- [-1, 0, 1]
      , y + dy >= 0
      , y + dy < maxY
      ]

    maxX = widthArray' arr
    maxY = heightArray' arr

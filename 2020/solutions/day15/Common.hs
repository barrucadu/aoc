{-# LANGUAGE BangPatterns #-}

module Common where

import           Control.Monad.ST            (runST)
import           Data.Foldable               (for_)

import qualified Data.Vector.Unboxed.Mutable as VUM

import           Utils                       (parseDigit)

parse :: String -> [Int]
parse = go . head . lines where
  go [] = []
  go (',':ds) = go ds
  go (d:ds) = parseDigit d : go ds

solve :: Int -> [Int] -> Int
solve target xs0 = runST $ do
    v <- VUM.replicate (maximum (target:xs0)) 0
    for_ (zip xs0 [1..]) $ \(x, i) -> VUM.write v x i
    go v (last xs0) (length xs0)
  where
    go v !prev !turn = do
      prevTurn <- VUM.read v prev
      let num = if prevTurn == 0 then 0 else turn - prevTurn
      if turn+1 == target
        then pure num
        else VUM.write v prev turn >> go v num (turn+1)

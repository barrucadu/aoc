{-# LANGUAGE BangPatterns #-}

module Common where

import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as V

import Utils

parse :: String -> (Int, Int)
{-# INLINABLE parse #-}
parse = go1 0 where
  go1 !acc (' ':rest) = (acc, go2 0 (drop (length "players; last marble is worth ") rest))
  go1 !acc (d:rest) = go1 (stepParseInt acc d) rest
  go1 _ _ = error "invalid input"

  go2 !acc (' ':_) = acc
  go2 !acc (d:rest) = go2 (stepParseInt acc d) rest
  go2 _ _ = error "invalid input"

solve :: (Int, Int) -> Int
{-# INLINABLE solve #-}
solve (nplayers, lastmarble) = runST $ do
    scores <- V.new nplayers
    scores' <- go scores 22 1 0 (dqfromList [0])
    getMax scores'
  where
    go :: V.STVector s Int -> Int -> Int -> Int -> Dequeue Int -> ST s (V.STVector s Int)
    go scores = go' where
      go' !countdown !marble !player dq
        | marble == lastmarble + 1 = pure scores
        | countdown == 0 = do
          let (m, dq') = dqpopFront (rotateA 7 dq)
          let dq'' = rotateC 1 dq'
          let score = marble + m
          let player' = player `mod` nplayers
          V.unsafeModify scores (+score) player'
          go' 22 (marble+1) (player'+1) dq''
        | otherwise = do
          let dq' = dqpushFront (rotateC 1 dq) marble
          go' (countdown-1) (marble+1) (player+1) dq'

    rotateA :: Int -> Dequeue a -> Dequeue a
    rotateA 0 dq = dq
    rotateA n dq =
      let (m, dq') = dqpopFront dq
      in rotateA (n-1) (dqpushBack dq' m)

    rotateC :: Int -> Dequeue a -> Dequeue a
    rotateC 0 dq = dq
    rotateC n dq =
      let (m, dq') = dqpopBack dq
      in rotateA (n-1) (dqpushFront dq' m)

    getMax v = getMax' (V.length v) 0 where
      getMax' 0 best = pure best
      getMax' idx best = do
        best' <- V.unsafeRead v (idx-1)
        getMax' (idx-1) (max best best')

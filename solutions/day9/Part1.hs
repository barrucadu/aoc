{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as V

import Utils

main :: IO ()
main = mainFor 9 parse (show . solve)

parse :: String -> (Int, Int)
parse = go1 0 where
  go1 !acc (' ':rest) = (acc, go2 0 (drop (length "players; last marble is worth ") rest))
  go1 !acc (d:rest) = go1 (stepParseInt acc d) rest
  go1 _ _ = error "invalid input"

  go2 !acc (' ':_) = acc
  go2 !acc (d:rest) = go2 (stepParseInt acc d) rest
  go2 _ _ = error "invalid input"

solve :: (Int, Int) -> Int
solve (nplayers, lastmarble) = runST $ do
    scores <- V.new nplayers
    scores' <- go scores 1 0 [] [0]
    getMax scores'
  where
    go :: V.STVector s Int -> Int -> Int -> [Int] -> [Int] -> ST s (V.STVector s Int)
    go scores = go' where
      go' !marble !player anticlockwise clockwise
        | marble == lastmarble + 1 = pure scores
        | marble `mod` 23 == 0 = do
          let (anticlockwise', (m:clockwise')) = rotateA 7 anticlockwise clockwise
          let (anticlockwise'', clockwise'') = rotateC 1 anticlockwise' clockwise'
          let score = marble + m
          let player' = (player + 1) `mod` nplayers
          let marble' = marble + 1
          V.unsafeModify scores (+score) player
          go' marble' player' anticlockwise'' clockwise''
        | otherwise = do
          let (anticlockwise', clockwise') = rotateC 1 anticlockwise clockwise
          let player' = (player + 1) `mod` nplayers
          let marble' = marble + 1
          go' marble' player' anticlockwise' (marble:clockwise')

    rotateA :: Int -> [Int] -> [Int] -> ([Int], [Int])
    rotateA _ [] [] = error "rotateA"
    rotateA 0 as cs = (as, cs)
    rotateA n as (c:cs) = rotateA (n-1) (c:as) cs
    rotateA n as [] = rotateA n [] (reverse as)

    rotateC :: Int -> [Int] -> [Int] -> ([Int], [Int])
    rotateC _ [] [] = error "rotateC"
    rotateC 0 as cs = (as, cs)
    rotateC n (a:as) cs = rotateC (n-1) as (a:cs)
    rotateC n [] cs = rotateC n (reverse cs) []

    getMax v = getMax' (V.length v) 0 where
      getMax' 0 best = pure best
      getMax' idx best = do
        best' <- V.unsafeRead v (idx-1)
        getMax' (idx-1) (max best best')

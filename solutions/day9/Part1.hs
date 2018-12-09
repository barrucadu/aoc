{-# LANGUAGE BangPatterns #-}

import Data.IntMap.Strict as M

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
solve (nplayers, lastmarble) = maximum (M.elems (go scores0 1 0 [] [0])) where
  go :: M.IntMap Int -> Int -> Int -> [Int] -> [Int] -> M.IntMap Int
  go scores !marble !player anticlockwise clockwise
    | marble == lastmarble + 1 = scores
    | marble `mod` 23 == 0 =
      let (anticlockwise', (m:clockwise')) = rotateA 7 anticlockwise clockwise
          (anticlockwise'', clockwise'') = rotateC 1 anticlockwise' clockwise'
          score = marble + m
          scores' = M.insertWith (+) player score scores
          player' = (player + 1) `mod` nplayers
          marble' = marble + 1
      in go scores' marble' player' anticlockwise'' clockwise''
    | otherwise =
      let (anticlockwise', clockwise') = rotateC 1 anticlockwise clockwise
          player' = (player + 1) `mod` nplayers
          marble' = marble + 1
      in go scores marble' player' anticlockwise' (marble:clockwise')

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

  scores0 = M.fromList [(player, 0) | player <- [0..nplayers-1]]

{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as S

import           Common
import           Utils

main :: IO ()
main = mainFor 10 parse (prettyPrint . solve)

solve :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
solve points0 = map ($go t0) funcs where
  go !t
    | yrange t <= 15 = t
    | otherwise = go (t+1)

  funcs = map func points0 where
    func ((x0, y0), (dx, dy)) t = (x0 + t * dx, y0 + t * dy)

  yrange t =
    let points = map ($t) funcs
        (ymin, ymax) = minmax (map snd points)
    in abs (ymax - ymin)

  t0 = 10000 -- looks good from my input

prettyPrint :: [(Int, Int)] -> String
prettyPrint points = unlines (rows ymin) where
  rows y
    | y == ymax + 1 = []
    | otherwise = [if (x, y) `S.member` spoints then '#' else ' ' | x <- [xmin..xmax]] : rows (y+1)

  spoints = S.fromList points

  (xmin, xmax) = minmax (map fst points)
  (ymin, ymax) = minmax (map snd points)

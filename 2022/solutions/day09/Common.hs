module Common where

import qualified Data.IntSet        as S
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty(..))

import           Utils              (parseInt)

type P = (Int, Int)
type Step = P -> P

parse :: String -> [Step]
parse = concatMap go . lines where
  go (d:' ':n) = replicate (parseInt n) $ movef d

  movef 'U' (x, y) = (x, y + 1)
  movef 'D' (x, y) = (x, y - 1)
  movef 'R' (x, y) = (x + 1, y)
  movef 'L' (x, y) = (x - 1, y)

solveGeneric :: Int -> [Step] -> Int
solveGeneric segments = S.size . fst . foldl' go (S.singleton (toInt origin), origin :| replicate segments origin) where
  go :: (S.IntSet, NonEmpty P) -> Step -> (S.IntSet, NonEmpty P)
  go (seen, hxy:|txys) step = (seen', hxy':|txys') where
    hxy' = step hxy

    (seen', txys') = moveTails [] hxy' txys

    moveTails :: [P] -> P -> [P] -> (S.IntSet, [P])
    moveTails acc prior (txy:rest) =
      let txy' = yank prior txy
      in moveTails (txy':acc) txy' rest
    moveTails acc prior [] = (S.insert (toInt prior) seen, reverse acc)

  origin :: P
  origin = (0, 0)

  toInt :: P -> Int
  toInt (x, y) = x * 1000000 + y

yank :: P -> P -> P
yank (hx, hy) (tx, ty)
    | adx == 2 && ady == 2 = (tx + signum dx, ty + signum dy)
    | adx == 2 = (tx + signum dx, hy)
    | ady == 2 = (hx, ty + signum dy)
    | otherwise = (tx, ty)
  where
    adx = abs dx
    ady = abs dy

    dx = hx - tx
    dy = hy - ty

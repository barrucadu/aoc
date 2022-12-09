module Common where

import           Data.List (foldl')
import qualified Data.Set  as S

import           Utils     (parseInt)

type P = (Int, Int)

data Dir = U | D | R | L
  deriving Eq

parse :: String -> [(Dir, Int)]
parse = map go . lines where
  go ('U':' ':n) = (U, parseInt n)
  go ('D':' ':n) = (D, parseInt n)
  go ('R':' ':n) = (R, parseInt n)
  go ('L':' ':n) = (L, parseInt n)

solveGeneric :: Int -> [(Dir, Int)] -> Int
solveGeneric segments = (\(seen, _, _) -> S.size seen) . foldl' go (S.singleton origin, origin, replicate segments origin) where
  go :: (S.Set P, P, [P]) -> (Dir, Int) -> (S.Set P, P, [P])
  go (seen, hxy, txys) (_, 0) = (seen, hxy, txys)
  go (seen, (hx, hy), txys) (dir, n) = go (seen', hxy', txys') (dir, n-1) where
    hxy' = case dir of
      U -> (hx, hy+1)
      D -> (hx, hy-1)
      R -> (hx+1, hy)
      L -> (hx-1, hy)

    (seen', txys') = moveTails [] hxy' txys

    moveTails :: [P] -> P -> [P] -> (S.Set P, [P])
    moveTails acc prior (txy:rest) =
      let txy' = yank prior txy
      in moveTails (txy':acc) txy' rest
    moveTails acc prior [] = (S.insert prior seen, reverse acc)

  origin :: P
  origin = (0, 0)

yank :: P -> P -> P
yank (hx, hy) (tx, ty)
    | adx == 2 && ady == 2 = (tx + sdx, ty + sdy)
    | adx == 2 = (tx + sdx, hy)
    | ady == 2 = (hx, ty + sdy)
    | otherwise = (tx, ty)
  where
    adx = abs dx
    ady = abs dy

    sdx = if dx < 0 then -1 else 1
    sdy = if dy < 0 then -1 else 1

    dx = hx - tx
    dy = hy - ty

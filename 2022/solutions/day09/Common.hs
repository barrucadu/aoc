module Common where

import           Data.List (foldl')
import qualified Data.Set  as S

import           Utils     (parseInt)

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
solveGeneric segments = (\(seen, _, _) -> S.size seen) . foldl' go (S.singleton origin, origin, replicate segments origin) where
  go :: (S.Set P, P, [P]) -> Step -> (S.Set P, P, [P])
  go (seen, hxy, txys) step = (seen', hxy', txys') where
    hxy' = step hxy

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

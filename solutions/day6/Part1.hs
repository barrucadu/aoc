import qualified Data.Map.Strict as M
import Data.List (sort)

import Common
import Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

solve :: (Int, Int, Int, Int, [(Int, Int)]) -> Int
solve (xmin, xmax, ymin, ymax, points) = go initial [(x, y) | x <- [xmin..xmax], y <- [ymin..ymax]] where
  go acc [] = maximum (M.elems acc)
  go acc (xy@(x, y):rest) = case closest xy of
    Just p
      | x == xmin || x == xmax || y == ymin || y == ymax -> go (M.delete p acc) rest
      | otherwise -> go (M.adjust (+1) p acc) rest
    Nothing -> go acc rest

  closest xy = case sort [(manhattan p xy, p) | p <- points] of
    ((a, _):(b, _):_) | a == b -> Nothing
    ((_, p):_) -> Just p
    _ -> Nothing

  initial = M.fromList [(p, 0) | p <- points]

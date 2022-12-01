{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 9 parse (show . solve)

solve :: [Int] -> Int
solve ints0 = search ints0 where
  target :: Int
  target = part1 ints0

  search :: [Int] -> Int
  search ns@(a:b:cs) = case search' (target - a - b) a b cs of
    Just soln -> soln
    Nothing   -> search (tail ns)
  search _ = error "no solution"

  search' :: Int -> Int -> Int -> [Int] -> Maybe Int
  search' !acc !lo !hi (x:xs) =
    let acc' = acc - x
        lo' = min lo x
        hi' = max hi x
    in if | acc' < 0  -> Nothing
          | acc' == 0 -> Just (lo' + hi')
          | otherwise -> search' acc' lo' hi' xs
  search' !acc !lo !hi []
    | acc == 0  = Just (lo + hi)
    | otherwise = Nothing

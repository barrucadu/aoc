module Common where

import Data.Sequence (Seq(Empty, (:<|)), (|>))
import qualified Data.Sequence as Sq

import Utils

parse :: String -> [Int]
parse = map parseInt . lines

part1 :: [Int] -> Int
part1 ints0 = let (start, end) = splitAt 25 ints0 in go (Sq.fromList start) end where
  go last25 (n:ns)
    | check last25 n = go (Sq.drop 1 last25 |> n) ns
    | otherwise = n
  go _ _ = error "no solution"

  check (x:<|xs) n = check' xs || check xs n where
    check' (y:<|ys)
      | x + y == n = True
      | otherwise  = check' ys
    check' Empty = False
  check Empty _ = False

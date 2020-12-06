module Common where

import qualified Data.Set as S

parse :: (S.Set Char -> S.Set Char -> S.Set Char) -> String -> [S.Set Char]
parse merge = parse' . lines where
  parse' (l:ls) = go (S.fromList l) ls
  parse' [] = []

  go acc ([]:(l:ls)) = acc : go (S.fromList l) ls
  go acc (l:ls) = go (acc `merge` S.fromList l) ls
  go acc [] = [acc]

solve :: [S.Set a] -> Int
solve = sum . map S.size

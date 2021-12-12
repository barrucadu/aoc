import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)

import Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

parse :: String -> [String]
parse = lines

solve :: [String] -> Int
solve = median . sort . mapMaybe (go []) where
  go [] [] = Nothing
  go as [] = Just (foldl' score 0 as)
  go ('(':as) (')':bs) = go as bs
  go ('[':as) (']':bs) = go as bs
  go ('{':as) ('}':bs) = go as bs
  go ('<':as) ('>':bs) = go as bs
  go _ (')':_) = Nothing
  go _ (']':_) = Nothing
  go _ ('}':_) = Nothing
  go _ ('>':_) = Nothing
  go as (b:bs) = go (b:as) bs

  score acc '(' = 5 * acc + 1
  score acc '[' = 5 * acc + 2
  score acc '{' = 5 * acc + 3
  score acc '<' = 5 * acc + 4
  score _ _ = error "invalid input"

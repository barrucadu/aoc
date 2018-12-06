import qualified Data.IntSet as S

import Utils

main :: IO ()
main = mainFor 1 (parseInts . lines) (show . solve)

solve :: [Int] -> Int
solve = go (S.singleton 0) 0 . cycle where
  go seen acc (n:ns) =
    let acc' = acc + n
    in if acc' `S.member` seen
       then acc'
       else go (S.insert acc' seen) acc' ns
  go _ _ _ = error "reached end of infinite list"

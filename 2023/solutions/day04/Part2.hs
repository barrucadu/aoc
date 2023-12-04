{-# LANGUAGE BangPatterns #-}

import qualified Data.IntMap as M

import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

solve :: [Int] -> Int
solve ts0 = go copies0 1 ts0 where
  copies0 = M.fromAscList [(n, 1) | n <- [1..length ts0]]

  go copies !n (matches:ts) =
    let mine = copies M.! n
        diff = M.fromAscList [(n, mine) | n <- [n+1..n+matches]]
        copies' = M.unionWith (+) copies diff
    in go copies' (n+1) ts
  go copies _ [] = sum $ M.elems copies

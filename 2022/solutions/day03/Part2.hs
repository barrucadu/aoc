import           Data.Char   (ord)
import qualified Data.IntSet as S

import           Utils

main :: IO ()
main = mainFor 3 solve show

solve :: String -> Int
solve = go . lines where
  go (l1:l2:l3:ls) = priority l1 l2 l3 + go ls
  go _ = 0

  priority l1 l2 l3 =
    let s1 = S.fromList (map toInt l1)
        s2 = S.fromList (map toInt l2)
        s3 = S.fromList (map toInt l3)
        common = S.intersection (S.intersection s1 s2) s3
    in head (S.toList common)

  toInt c =
    let n = ord c - 64 -- A => 1, a => 33
    in if n >= 33 then n - 32 else n + 26

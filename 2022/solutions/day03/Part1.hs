import           Data.Char   (ord)
import qualified Data.IntSet as S

import           Utils

main :: IO ()
main = mainFor 3 solve show

solve :: String -> Int
solve = sum . map go . lines where
  go l =
    let mid = length l `div` 2
        (l1, l2) = splitAt mid l
        s1 = S.fromList (map toInt l1)
        s2 = S.fromList (map toInt l2)
        common = S.intersection s1 s2
    in head (S.toList common)

  toInt c =
    let n = ord c - 64 -- A => 1, a => 33
    in if n >= 33 then n - 32 else n + 26

import           Data.List (sort)

import           Common
import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: [(Packet, Packet)] -> Int
solve ps0 = product [n | (n, p) <- zip [1..] ps, p == div1 || p == div2] where
  ps = sort $ div1 : div2 : concatMap (\(l, r) -> [l, r]) ps0

  div1 = [AList [AInt 2]]
  div2 = [AList [AInt 6]]

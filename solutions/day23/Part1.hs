import           Data.List (maximumBy)
import           Data.Ord  (comparing)

import           Common
import           Utils

main :: IO ()
main = mainFor 23 parse (show . solve)

solve :: [((Int, Int, Int), Int)] -> Int
solve input0 = length (filter inRange input0) where
  (xy0, r0) = maximumBy (comparing snd) input0

  inRange (xy, _) = manhattan3 xy0 xy <= r0

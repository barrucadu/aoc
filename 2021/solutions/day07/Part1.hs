import Data.List (sort)

import Common
import Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: [Int] -> Int
solve positions = sum $ map (\pos -> abs (target - pos)) positions where
  target = median (sort positions)

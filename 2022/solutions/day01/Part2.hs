import           Data.List (sort)

import           Common
import           Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

solve :: [Int] -> Int
solve ns =
  let (a:b:c:_) = reverse (sort ns)
  in a + b + c

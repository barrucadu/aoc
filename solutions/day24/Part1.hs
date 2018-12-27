import           Data.Maybe (fromJust)

import           Common
import           Utils

main :: IO ()
main = mainFor 24 parse (show . solve)

solve :: ([Army], [Army]) -> Int
solve = fst . fromJust . fight

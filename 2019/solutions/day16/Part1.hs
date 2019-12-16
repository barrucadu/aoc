import           Data.Char (intToDigit)

import           Common
import           Utils

main :: IO ()
main = mainFor 16 parse (map intToDigit . solve)

solve :: [Int] -> [Int]
solve = take 8 . fftN 100

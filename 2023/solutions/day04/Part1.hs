import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

solve :: [Int] -> Int
solve = sum . map score where
  score matches = (2 ^ matches) `div` 2

import           Common
import           Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

solve :: [Int] -> Int
solve = maximum

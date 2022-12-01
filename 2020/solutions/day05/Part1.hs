import           Common
import           Utils

main :: IO ()
main = mainFor 5 parse (show . solve)

solve :: [Int] -> Int
solve = maximum

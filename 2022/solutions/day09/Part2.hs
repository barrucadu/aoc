import           Common
import           Utils

main :: IO ()
main = mainFor 9 parse (show . solve)

solve :: [Step] -> Int
solve = solveGeneric 9

import           Common
import           Utils

main :: IO ()
main = mainFor 9 parse (show . solve)

solve :: [(Dir, Int)] -> Int
solve = solveGeneric 1

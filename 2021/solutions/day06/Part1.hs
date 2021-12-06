import Common
import Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

solve :: [Int] -> Int
solve = countFishAtDay 80

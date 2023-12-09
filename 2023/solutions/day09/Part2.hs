import           Common
import           Utils

main :: IO ()
main = mainFor 9 parse (show . solve)

solve :: [[Int]] -> Int
solve = sum . map (extrapolate . reverse)

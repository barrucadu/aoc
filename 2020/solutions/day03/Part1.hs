import           Common
import           Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

solve :: TreeMap -> Int
solve trees = slope trees (-3) (-1)

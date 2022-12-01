import           Common
import           Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

solve :: TreeMap -> Int
solve trees =
  slope trees (-1) (-1) *
  slope trees (-3) (-1) *
  slope trees (-5) (-1) *
  slope trees (-7) (-1) *
  slope trees (-1) (-2)

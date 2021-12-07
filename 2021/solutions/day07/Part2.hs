import Common
import Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: [Int] -> Int
solve positions = sum $ map (\pos -> sumFromOneToN $ abs (target - pos)) positions where
  sumFromOneToN n = (n * (n + 1)) `div` 2
  target = sum positions `div` length positions

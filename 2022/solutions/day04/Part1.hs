import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

solve :: [(P, P)] -> Int
solve = length . filter check where
  check (ab, cd) = ab `inside` cd || cd `inside` ab

  (P a b) `inside` (P c d) = a >= c && b <= d

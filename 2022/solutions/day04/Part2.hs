import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

solve :: [(P, P)] -> Int
solve = length . filter check where
  check (ab, cd) = ab `overlaps` cd || cd `overlaps` ab

  (P a b) `overlaps` (P c d) =
    (a >= c && a <= d) ||
    (b >= c && b <= d)

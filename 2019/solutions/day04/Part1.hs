import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

solve :: (Int, Int) -> Int
solve = length . filter check . generate where
  check (a, b, c, d, e, f) =
    a == b || b == c || c == d || d == e || e == f

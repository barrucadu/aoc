import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

solve :: (Int, Int) -> Int
solve = length . filter check . generate where
  check (a, b, c, d, e, f) =
    (a == b && c /= b) ||
    (b == c && (a /= b && d /= c)) ||
    (c == d && (b /= c && e /= d)) ||
    (d == e && (c /= d && f /= e)) ||
    (e == f && d /= e)

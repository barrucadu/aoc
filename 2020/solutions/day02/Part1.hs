import Common
import Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

solve :: [(Int, Int, Char, String)] -> Int
solve = length . filter validate where
  validate (lo, hi, c, password) =
    let len = length $ filter (==c) password
    in len >= lo && len <= hi

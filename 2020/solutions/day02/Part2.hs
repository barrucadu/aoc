import Common
import Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

solve :: [(Int, Int, Char, String)] -> Int
solve = length . filter validate where
  validate (i, j, c, password) =
    let ok1 = password !! (i-1) == c
        ok2 = password !! (j-1) == c
    in (ok1 && not ok2) || (not ok1 && ok2)

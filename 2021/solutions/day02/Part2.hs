import Data.List (foldl')
import Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

parse :: String -> (Int, Int, Int)
parse = foldl' go (0, 0, 0) . lines where
  go (x, y, aim) ('f':'o':'r':'w':'a':'r':'d':' ':ds) = let d = parseInt ds in (x + d, y + aim * d, aim)
  go (x, y, aim) ('d':'o':'w':'n':' ':ds) = (x, y, aim + parseInt ds)
  go (x, y, aim) ('u':'p':' ':ds) = (x, y, aim - parseInt ds)
  go _ _ = error "bad input"

solve :: (Int, Int, Int) -> Int
solve (x, y, _) = x * y

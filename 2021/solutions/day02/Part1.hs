import Data.List (foldl')
import Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

parse :: String -> (Int, Int)
parse = foldl' go (0, 0) . lines where
  go (x, y) ('f':'o':'r':'w':'a':'r':'d':' ':ds) = (x + parseInt ds, y)
  go (x, y) ('d':'o':'w':'n':' ':ds) = (x, y + parseInt ds)
  go (x, y) ('u':'p':' ':ds) = (x, y - parseInt ds)
  go _ _ = error "bad input"

solve :: (Int, Int) -> Int
solve = uncurry (*)

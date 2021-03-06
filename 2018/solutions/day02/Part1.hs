import           Data.List (foldl', group, sort)

import           Utils

main :: IO ()
main = mainFor 2 lines (show . solve)

solve :: [String] -> Int
solve input = twos * threes where
  (twos, threes) = foldl' go (0, 0) input

  go (tw, th) line =
    let counts = map length (group (sort line))
    in ( tw + if 2 `elem` counts then 1 else 0
       , th + if 3 `elem` counts then 1 else 0
       )

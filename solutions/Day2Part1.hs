import Data.List (foldl', group, sort)

main :: IO ()
main = do
  input <- lines <$> readFile "../inputs/day2.txt"
  print (solve input)

solve :: [String] -> Int
solve input = twos * threes where
  (twos, threes) = foldl' go (0, 0) input

  go (tw, th) line =
    let counts = map length (group (sort line))
    in ( tw + if any (==2) counts then 1 else 0
       , th + if any (==3) counts then 1 else 0
       )

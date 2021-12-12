import Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

parse :: String -> [String]
parse = lines

solve :: [String] -> Int
solve = sum . map (go []) where
  go ('(':as) (')':bs) = go as bs
  go ('[':as) (']':bs) = go as bs
  go ('{':as) ('}':bs) = go as bs
  go ('<':as) ('>':bs) = go as bs
  go _ (')':_) = 3
  go _ (']':_) = 57
  go _ ('}':_) = 1197
  go _ ('>':_) = 25137
  go as (b:bs) = go (b:as) bs
  go _ _ = 0

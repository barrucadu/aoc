import           Utils

main :: IO ()
main = mainFor 6 solve show

solve :: String -> Int
solve = go 0 where
  go pos (x:xs)
    | isMarker (x : take 13 xs) = pos + 14
    | otherwise = go (pos+1) xs
  go _ _ = error "no solution!"

  isMarker = go' [] where
    go' _ [] = True
    go' acc (x:xs) = x `notElem` acc && go' (x:acc) xs

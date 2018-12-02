main :: IO ()
main = do
  input <- lines <$> readFile "../inputs/day2.txt"
  putStrLn (solve input)

solve :: [String] -> String
solve (l:ls) = case findMatch l ls of
    Just common -> common
    Nothing -> solve ls
  where
    findMatch k0 = go where
      go (k:ks) = case checkMatch k0 k of
        Nothing -> go ks
        res -> res
      go [] = Nothing

    checkMatch = go [] where
      go acc (a:as) (b:bs)
        | a == b = go (a:acc) as bs
        | otherwise = go' acc as bs
      go _ _ _ = Nothing

      go' acc (a:as) (b:bs)
        | a == b = go' (a:acc) as bs
        | otherwise = Nothing
      go' acc [] [] = Just (reverse acc)
      go' _ _ _ = Nothing
solve _ = error "didn't find a match"

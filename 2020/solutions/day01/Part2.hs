import Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

parse :: String -> [Int]
parse = map parseInt . lines

solve :: [Int] -> Int
solve (a:as) = solve2 (restrict a as) where
  solve2 (b:bs) = solve3 (restrict b bs) where
    solve3 (c:cs)
      | a + b + c == target = a * b * c
      | otherwise = solve3 cs
    solve3 [] = solve2 bs
  solve2 [] = solve as

  restrict x = filter (\y -> target - x - y > 0)
solve [] = error "no solution"

target :: Int
target = 2020

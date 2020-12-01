import Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

parse :: String -> [Int]
parse = map parseInt . lines

solve :: [Int] -> Int
solve (a:as) = solve' as where
  solve' (b:bs)
    | a + b == target = a * b
    | otherwise = solve' bs
  solve' [] = solve as
solve [] = error "no solution"

target :: Int
target = 2020

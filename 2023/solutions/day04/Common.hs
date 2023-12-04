module Common where

parse :: String -> [Int]
parse = map (go . words) . lines where
  go (_:_:ns) = go' [] ns

  go' winners ("|":rest) = length $ filter (`elem` winners) rest
  go' winners (n:rest) = go' (n:winners) rest

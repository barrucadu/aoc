{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 8 parse show

parse :: String -> Int
parse = sum . map go . lines where
  go :: String -> Int
  go ('|':' ':rest) = go' 0 0 rest
  go (_:rest) = go rest
  go _ = error "invalid input"

  go' :: Int -> Int -> String -> Int
  go' !n !c []
    | check c = n + 1
    | otherwise = n
  go' !n !c (' ':rest)
    | check c = go' (n+1) 0 rest
    | otherwise = go' n 0 rest
  go' !n !c (_:rest) = go' n (c+1) rest

  check :: Int -> Bool
  check 2 = True -- one
  check 4 = True -- four
  check 3 = True -- seven
  check 7 = True -- eight
  check _ = False

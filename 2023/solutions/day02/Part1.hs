{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 2 solve show

solve :: String -> Int
solve = go 0 . map words . lines where
  go !acc ((_:num:configs):ls)
    | check configs = go (acc + parseInt (init num)) ls
    | otherwise = go acc ls
  go !acc _ = acc

  check (num:('r':_):rest) = parseInt num <= 12 && check rest
  check (num:('g':_):rest) = parseInt num <= 13 && check rest
  check (num:('b':_):rest) = parseInt num <= 14 && check rest
  check _ = True

{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils (parseInt)

parse :: String -> [Int]
parse = go 0 . lines where
  go !n [] = [n]
  go !n ("":ls) = n : go 0 ls
  go !n (l:ls) = go (n + parseInt l) ls

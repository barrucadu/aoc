{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils (stepParseInt)

parse :: String -> [Int]
parse = go . lines where
  go :: [String] -> [Int]
  go [l] = go' 0 l
  go _ = error "invalid input"

  go' :: Int -> String -> [Int]
  go' !n [] = [n]
  go' !n (',':rest) = n : go' 0 rest
  go' !n (c:rest) = go' (stepParseInt n c) rest

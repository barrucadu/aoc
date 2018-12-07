{-# LANGUAGE BangPatterns #-}

module Common where

import Utils

parse :: String -> [(Int, Int)]
{-# INLINABLE parse #-}
parse input0 = map (go 0) (lines input0) where
  go !acc (',':' ':rest) = (acc, go1 0 rest)
  go !acc (d:rest) = go (stepParseInt acc d) rest
  go _ [] = error "invalid input"

  go1 !acc [] = acc
  go1 !acc (d:rest) = go1 (stepParseInt acc d) rest

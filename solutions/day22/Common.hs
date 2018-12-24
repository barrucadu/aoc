{-# LANGUAGE BangPatterns #-}

module Common where

import Utils

parse :: String -> (Int, (Int, Int))
{-# INLINABLE parse #-}
parse input0 = (depth, target) where
  (dline:tline:_) = lines input0

  depth  = parseInt (drop (length "depth: ")  dline)
  target = go 0 (drop (length "target: ") tline)

  go !acc (',':rest) = (acc, parseInt rest)
  go !acc (c:rest) = go (stepParseInt acc c) rest
  go _ _ = error "invalid input"

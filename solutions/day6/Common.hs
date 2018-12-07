{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.Vector.Unboxed as V

import Utils

parse :: String -> V.Vector (Int, Int)
{-# INLINABLE parse #-}
parse = V.fromList . map (go 0) . lines where
  go !acc (',':' ':rest) = (acc, go1 0 rest)
  go !acc (d:rest) = go (stepParseInt acc d) rest
  go _ [] = error "invalid input"

  go1 !acc [] = acc
  go1 !acc (d:rest) = go1 (stepParseInt acc d) rest

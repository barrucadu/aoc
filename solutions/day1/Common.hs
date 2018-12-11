module Common where

import Utils

parse :: String -> [Int]
{-# INLINABLE parse #-}
parse = map go . lines where
  go ('+':n) = parseInt n
  go ('-':n) = -1 * parseInt n
  go _ = error "expected +\\d+ or -\\d+"

{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils

parse :: String -> (Int, Int)
{-# INLINE parse #-}
parse = goL 0 where
  goL !acc ('-':rest) = (acc, goR 0 rest)
  goL !acc (c:rest) = goL (stepParseInt acc c) rest
  goL _ _ = error "unexpected end of input"

  goR !acc ('\n':_) = acc
  goR !acc (c:rest) = goR (stepParseInt acc c) rest
  goR !acc [] = acc

generate :: (Int, Int) -> [(Int, Int, Int, Int, Int, Int)]
{-# INLINE generate #-}
generate (lo, hi) =
  [ (a, b, c, d, e, f)
  | a <- [lo `div` 100000..min 9 (hi `div` 100000 + 1)]
  , b <- [a..9]
  , c <- [b..9]
  , d <- [c..9]
  , e <- [d..9]
  , f <- [e..9]
  , let val = a * 100000 + b * 10000 + c * 1000 + d * 100 + e * 10 + f
  , val >= lo
  , val <= hi
  ]

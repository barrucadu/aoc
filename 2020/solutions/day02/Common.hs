{-# LANGUAGE BangPatterns #-}

module Common where

import Utils

parse :: String -> [(Int, Int, Char, String)]
parse = map (go1 0) . lines where
  go1 :: Int -> String -> (Int, Int, Char, String)
  go1 !acc ('-':cs) = go2 acc 0 cs
  go1 !acc (c:cs) = go1 (stepParseInt acc c) cs
  go1 _ _ = error "invalid input"

  go2 :: Int -> Int -> String -> (Int, Int, Char, String)
  go2 !lo !acc (' ':c:':':' ':cs) = (lo, acc, c, cs)
  go2 !lo !acc (c:cs) = go2 lo (stepParseInt acc c) cs
  go2 _ _ _  = error "invalid input"

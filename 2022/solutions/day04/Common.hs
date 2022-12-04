{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils (stepParseInt)

data P = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int

parse :: String -> [(P, P)]
parse = map go . lines where
  go = go11 0

  go11 !a ('-':xs) = go12 a 0 xs
  go11 !a (x:xs) = go11 (stepParseInt a x) xs

  go12 !a !b (',':xs) = go21 a b 0 xs
  go12 !a !b (x:xs) = go12 a (stepParseInt b x) xs

  go21 !a !b !c ('-':xs) = go22 a b c 0 xs
  go21 !a !b !c (x:xs) = go21 a b (stepParseInt c x) xs

  go22 !a !b !c !d [] = (P a b, P c d)
  go22 !a !b !c !d (x:xs) = go22 a b c (stepParseInt d x) xs

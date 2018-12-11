{-# LANGUAGE BangPatterns #-}

import qualified Data.IntSet as S
import Data.Char (chr, ord)

import Utils

main :: IO ()
main = mainFor 5 init (show . solve)

solve :: String -> Int
solve input0 = minimum [ count | p <- S.toList polymers, let (count, _) = shrink (remove p input) ] where
  (_, input) = shrink input0

  polymers = S.map lowercase' (go S.empty input) where
    go acc (p:ps) = go (S.insert (ord p) acc) ps
    go acc [] = acc

  remove p = filter (\c -> ord (lowercase c) /= p)

  shrink = go 0 [] where
    go !n [] (b:bs) = go (n+1) [b] bs
    go !n as [] = (n, as)
    go !n as0@(a:as) (b:bs)
      | a /= b && lowercase a == lowercase b = go (n-1) as bs
      | otherwise = go (n+1) (b:as0) bs

  lowercase' = ord . lowercase . chr

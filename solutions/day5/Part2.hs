{-# LANGUAGE BangPatterns #-}

import qualified Data.IntSet as S
import Data.Char (chr, ord)

import Utils

main :: IO ()
main = do
  input <- init <$> readFile "../inputs/day5.txt"
  print (solve input)

solve :: String -> Int
solve input = minimum [ shrink (remove p input) | p <- S.toList polymers ] where
  polymers = S.map lowercase' (go S.empty input) where
    go acc (p:ps) = go (S.insert (ord p) acc) ps
    go acc [] = acc

  remove p = filter (\c -> ord (lowercase c) /= p)

  shrink = go 0 [] where
    go !n [] (b:bs) = go (n+1) [b] bs
    go !n _ [] = n
    go !n as0@(a:as) (b:bs)
      | a /= b && lowercase a == lowercase b = go (n-1) as bs
      | otherwise = go (n+1) (b:as0) bs

  lowercase' = ord . lowercase . chr

{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 13 parse solve

solve :: (S.Set Point, [Fold]) -> String
solve = prettyPrint . uncurry go where
  go ps [] = ps
  go ps (f:fs) = go (fold f ps) fs

prettyPrint :: S.Set Point -> String
prettyPrint ps = go 0 0 where
  go !x !y
    | y > maxY = []
    | x > maxX = '\n' : go 0 (y+1)
    | P x y `S.member` ps = '#' : go (x+1) y
    | otherwise = ' ' : go (x+1) y

  maxX = maximum [x | P x _ <- S.toList ps]
  maxY = maximum [y | P _ y <- S.toList ps]

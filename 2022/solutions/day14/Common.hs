{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.IntSet        as S
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty(..))

import           Utils              (stepParseInt)

type Point = (Int, Int)
type Path = NonEmpty Point

parse :: String -> [Path]
parse = map parsePath . lines where
  parsePath l =
    let (start, rest) = parseP l
    in start :| parsePs rest

  parsePs [] = []
  parsePs ('-':'>':' ':l) = parsePs l
  parsePs l = let (p, rest) = parseP l in p : parsePs rest

  parseP l =
    let (x, l') = parseN 0 l
        (y, rest) = parseN 0 l'
    in ((x, y), rest)

  parseN !acc [] = (acc, [])
  parseN !acc (',':rest) = (acc, rest)
  parseN !acc (' ':rest) = (acc, rest)
  parseN !acc (x:xs) = parseN (stepParseInt acc x) xs

toPoints :: [Path] -> (S.IntSet, Int)
toPoints paths = (foldl' path S.empty paths, maxY) where
  maxY = maximum [y | (p:|ps) <- paths, (_, y) <- (p:ps)]

  path points ((x0, y0):|ps) =
    let (points', _, _) = foldl' line (points, x0, y0) ps
    in points'

  line (points, x0, y0) (x, y) =
    let xrange = [min x0 x .. max x0 x]
        yrange = [min y0 y .. max y0 y]
        points' = S.fromList [pointToInt maxY (x', y') | x' <- xrange, y' <- yrange]
    in (points `S.union` points', x, y)

pointToInt :: Int -> Point -> Int
pointToInt maxY (x, y) = x * (maxY + 2) + y

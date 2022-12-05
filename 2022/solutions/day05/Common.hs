{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.IntMap as M
import           Data.List   (foldl')

import           Utils       (parseInt, stepParseInt)

type Stacks = M.IntMap [Char]

data Move = Move Int Int Int

parse :: String -> (Stacks, [Move])
parse = go . reverse . lines where
  go = parseMoves []

  parseMoves :: [Move] -> [String] -> (Stacks, [Move])
  parseMoves moves ("":_:stackdefs) = (parseStackDefs stackdefs, moves)
  parseMoves moves (m:movedefs) = parseMoves (parseMove m:moves) movedefs

  parseMove :: String -> Move
  parseMove (_:_:_:_:_:xs) = parseMoveN 0 xs

  parseMoveN :: Int -> String -> Move
  parseMoveN !n (' ':_:_:_:_:_:xs) = parseMoveFromTo n 0 xs
  parseMoveN !n (x:xs) = parseMoveN (stepParseInt n x) xs

  parseMoveFromTo :: Int -> Int -> String -> Move
  parseMoveFromTo !n !from (' ':_:_:_:to) = Move n from (parseInt to)
  parseMoveFromTo !n !from (x:xs) = parseMoveFromTo n (stepParseInt from x) xs

  parseStackDefs :: [String] -> Stacks
  parseStackDefs = foldl' parseStackDef emptyStacks

  parseStackDef :: Stacks -> String -> Stacks
  parseStackDef s (_:a:_:_:_:b:_:_:_:c:_:_:_:d:_:_:_:e:_:_:_:f:_:_:_:g:_:_:_:h:_:_:_:i:_) =
    (if a == ' ' then id else M.adjust (a:) 1) .
    (if b == ' ' then id else M.adjust (b:) 2) .
    (if c == ' ' then id else M.adjust (c:) 3) .
    (if d == ' ' then id else M.adjust (d:) 4) .
    (if e == ' ' then id else M.adjust (e:) 5) .
    (if f == ' ' then id else M.adjust (f:) 6) .
    (if g == ' ' then id else M.adjust (g:) 7) .
    (if h == ' ' then id else M.adjust (h:) 8) .
    (if i == ' ' then id else M.adjust (i:) 9) $ s

showStacks :: Stacks -> String
showStacks s =
  [ head (s M.! 1)
  , head (s M.! 2)
  , head (s M.! 3)
  , head (s M.! 4)
  , head (s M.! 5)
  , head (s M.! 6)
  , head (s M.! 7)
  , head (s M.! 8)
  , head (s M.! 9)
  ]

emptyStacks :: Stacks
emptyStacks = M.fromList
  [ (1, [])
  , (2, [])
  , (3, [])
  , (4, [])
  , (5, [])
  , (6, [])
  , (7, [])
  , (8, [])
  , (9, [])
  ]

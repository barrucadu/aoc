{-# LANGUAGE BangPatterns #-}

module Common where

import Utils (stepParseInt)

type Expr = [Atom]

data Atom
  = ANum {-# UNPACK #-} !Int
  | AOpAdd
  | AOpMul
  | ANested !Expr
  deriving (Eq, Ord, Read, Show)

parse :: String -> [Expr]
parse = map (fst . go []) . lines where
  go :: Expr -> String -> (Expr, String)
  go atoms [] = (reverse atoms, [])
  go atoms ('(':rest) =
    let (nested, rest') = go [] rest
    in go (ANested nested:atoms) rest'
  go atoms (')':rest) = (reverse atoms, rest)
  go atoms (' ':rest) = go atoms rest
  go atoms ('+':rest) = go (AOpAdd:atoms) rest
  go atoms ('*':rest) = go (AOpMul:atoms) rest
  go atoms rest =
    let (num, rest') = goNum 0 rest
    in go (ANum num:atoms) rest'

  goNum :: Int -> String -> (Int, String)
  goNum !acc rest@(' ':_) = (acc, rest)
  goNum !acc rest@(')':_) = (acc, rest)
  goNum !acc (d:ds) = goNum (stepParseInt acc d) ds
  goNum !acc [] = (acc, [])

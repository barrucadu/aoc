{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.Set as S

import           Utils    (parseInt, stepParseInt)

data Point = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Read, Show)

data Fold
  = FoldX {-# UNPACK #-} !Int
  | FoldY {-# UNPACK #-} !Int
  deriving (Eq, Ord, Read, Show)

parse :: String -> (S.Set Point, [Fold])
parse = go . lines where
  go ls =
    let (points, rest) = parsePoints S.empty ls
    in (points, map parseFold rest)

  parsePoints ps ([]:rest) = (ps, rest)
  parsePoints ps (l:ls) =
    let goN !n [] = (n, [])
        goN !n (',':rest) = (n, rest)
        goN !n (c:cs) = goN (stepParseInt n c) cs

        (x, rest) = goN 0 l
        (y, []) = goN 0 rest
    in parsePoints (S.insert (P x y) ps) ls
  parsePoints _ _ = error "invalid input"

  parseFold ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=':rest) = FoldX (parseInt rest)
  parseFold ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=':rest) = FoldY (parseInt rest)
  parseFold _ = error "invalid input"

fold :: Fold -> S.Set Point -> S.Set Point
fold (FoldX xF) = S.map go where
  go (P x y)
    | x > xF = P (xF - (x - xF)) y
    | otherwise = P x y
fold (FoldY yF) = S.map go where
  go (P x y)
    | y > yF = P x (yF - (y - yF))
    | otherwise = P x y

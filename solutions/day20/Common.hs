module Common where

import qualified Data.IntSet as S
import qualified Data.Graph.Inductive as G

parse :: String -> G.UGr
{-# INLINABLE parse #-}
parse input0 = go (S.singleton 0) [] 0 [] input0 where
  go :: S.IntSet -> [G.Edge] -> Int -> [Int] -> String -> G.UGr
  go vs es _ _ [] = G.mkUGraph (S.toList vs) es
  go vs es p ps ('\n':rest) = go vs es p ps rest
  go vs es p ps ('^':rest)  = go vs es p ps rest
  go vs es p ps ('$':rest)  = go vs es p ps rest
  go vs es p ps ('(':rest)  = go vs es p (p:ps) rest
  go vs es _ (p:ps) (')':rest) = go vs es p ps rest
  go vs es _ ps@(p:_) ('|':rest) = go vs es p ps rest
  go vs es p ps ('N':rest) = step vs es p (p - magic) ps rest
  go vs es p ps ('S':rest) = step vs es p (p + magic) ps rest
  go vs es p ps ('E':rest) = step vs es p (p - 1) ps rest
  go vs es p ps ('W':rest) = step vs es p (p + 1) ps rest
  go _ _ _ _ _ = error "invalid input"

  step vs es from to ps rest =
    let vs' = S.insert to vs
        es' = (from, to):es
    in go vs' es' to ps rest

  magic = 3 * length input0

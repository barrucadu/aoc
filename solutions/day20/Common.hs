module Common where

import qualified Data.IntSet as S
import qualified Data.Graph.Inductive as G

data Pos = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord)

parse :: String -> G.UGr
{-# INLINABLE parse #-}
parse input0 = go (S.singleton 0) [] (P 0 0) [] input0 where
  go :: S.IntSet -> [G.Edge] -> Pos -> [Pos] -> String -> G.UGr
  go vs es _ _ [] = G.mkUGraph (S.toList vs) es
  go vs es p ps ('\n':rest) = go vs es p ps rest
  go vs es p ps ('^':rest)  = go vs es p ps rest
  go vs es p ps ('$':rest)  = go vs es p ps rest
  go vs es p ps ('(':rest)  = go vs es p (p:ps) rest
  go vs es _ (p:ps) (')':rest) = go vs es p ps rest
  go vs es _ ps@(p:_) ('|':rest) = go vs es p ps rest
  go vs es p@(P x y) ps ('N':rest) = step vs es p (P x (y-1)) ps rest
  go vs es p@(P x y) ps ('S':rest) = step vs es p (P x (y+1)) ps rest
  go vs es p@(P x y) ps ('E':rest) = step vs es p (P (x-1) y) ps rest
  go vs es p@(P x y) ps ('W':rest) = step vs es p (P (x+1) y) ps rest
  go _ _ _ _ _ = error "invalid input"

  step vs es (P x y) p'@(P x' y') ps rest =
    let from = x  + y  * magic
        to   = x' + y' * magic
        vs'  = S.insert to vs
        es'  = (from, to):es
    in go vs' es' p' ps rest

  magic = 3 * length input0

module Common where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Graph.Inductive as G

data Pos = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord)

parse :: String -> G.UGr
{-# INLINABLE parse #-}
parse = go (M.singleton (P 0 0) 0) [] (P 0 0) [] where
  go :: M.Map Pos Int -> [G.Edge] -> Pos -> [Pos] -> String -> G.UGr
  go vs es _ _ [] = G.mkUGraph (M.elems vs) es
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

  step vs es p p' ps rest =
    let from = fromJust (M.lookup p vs)
        to = M.findWithDefault (M.size vs) p' vs
        vs' = M.insert p' to vs
        es' = (from, to):es
    in go vs' es' p' ps rest

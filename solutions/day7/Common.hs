module Common where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

parse :: String -> M.Map Char (Int, S.Set Char)
{-# INLINABLE parse #-}
parse = go M.empty where
  go g [] = g
  go g xs =
    let (from:xs') = drop (length "Step ") xs
        (to:xs'') = drop (length " must be finished before step ") xs'
        rest = drop (length " can begin.\n") xs''
    in go (M.alter inc to (M.alter (link to) from g)) rest

  link to (Just (x, successors)) = Just (x, S.insert to successors)
  link to Nothing = Just (0, S.singleton to)

  inc (Just (x, successors)) = Just (x+1, successors)
  inc Nothing = Just (1, S.empty)

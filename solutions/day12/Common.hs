module Common where

import qualified Data.Map.Strict as M

type Transitions = (M.Map String Char, M.Map String Char)

parse :: String -> (String, Transitions)
{-# INLINABLE parse #-}
parse = go1 . lines where
  go1 ((_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:rest):[]:transitions) = (rest, go2 M.empty M.empty transitions)
  go1 _ = error "invalid input"

  go2 trdot trhash ([l1, l2, '.', r1, r2, _, _, _, _, d]:rest) =
    go2 (M.insert [l1, l2, r1, r2] d trdot) trhash rest
  go2 trdot trhash ([l1, l2, '#', r1, r2, _, _, _, _, d]:rest) =
    go2 trdot (M.insert [l1, l2, r1, r2] d trhash) rest
  go2 trdot trhash _ = (trdot, trhash)

findTransition :: (M.Map String Char, M.Map String Char) -> Char -> String -> Char
{-# INLINABLE findTransition #-}
findTransition (m, _) '.' k = M.findWithDefault '.' k m
findTransition (_, m)  _  k = M.findWithDefault '.' k m

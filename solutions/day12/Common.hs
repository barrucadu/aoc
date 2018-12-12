module Common where

import qualified Data.Set as S

type Transitions = (S.Set String, S.Set String)

parse :: String -> (String, Transitions)
{-# INLINABLE parse #-}
parse = go1 . lines where
  go1 ((_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:rest):[]:transitions) = (rest, go2 S.empty S.empty transitions)
  go1 _ = error "invalid input"

  go2 trdot trhash ([l1, l2, '.', r1, r2, _, _, _, _, '#']:rest) =
    go2 (S.insert [l1, l2, r1, r2] trdot) trhash rest
  go2 trdot trhash ([l1, l2, '#', r1, r2, _, _, _, _, '#']:rest) =
    go2 trdot (S.insert [l1, l2, r1, r2] trhash) rest
  go2 trdot trhash (_:rest) = go2 trdot trhash rest
  go2 trdot trhash [] = (trdot, trhash)

findTransition :: Transitions -> Char -> String -> Char
{-# INLINABLE findTransition #-}
findTransition (s, _) '.' k
  | k `S.member` s = '#'
  | otherwise      = '.'
findTransition (_, s)  _  k
  | k `S.member` s = '#'
  | otherwise      = '.'

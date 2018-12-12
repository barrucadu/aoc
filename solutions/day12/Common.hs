module Common where

import qualified Data.IntSet as S

type Transitions = (S.IntSet, S.IntSet)

parse :: String -> (String, Transitions)
{-# INLINABLE parse #-}
parse = go1 . lines where
  go1 ((_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:rest):[]:transitions) = (rest, go2 S.empty S.empty transitions)
  go1 _ = error "invalid input"

  go2 trdot trhash ([l1, l2, '.', r1, r2, _, _, _, _, '#']:rest) =
    go2 (S.insert (toK l1 l2 r1 r2) trdot) trhash rest
  go2 trdot trhash ([l1, l2, '#', r1, r2, _, _, _, _, '#']:rest) =
    go2 trdot (S.insert (toK l1 l2 r1 r2) trhash) rest
  go2 trdot trhash (_:rest) = go2 trdot trhash rest
  go2 trdot trhash [] = (trdot, trhash)

findTransition :: Transitions -> Char -> Int -> Char
{-# INLINABLE findTransition #-}
findTransition (s, _) '.' k
  | k `S.member` s = '#'
  | otherwise      = '.'
findTransition (_, s)  _  k
  | k `S.member` s = '#'
  | otherwise      = '.'

toK :: Char -> Char -> Char -> Char -> Int
{-# INLINABLE toK #-}
toK '.' '.' '.' '.' = 0
toK '.' '.' '.'  _  = 1
toK '.' '.'  _  '.' = 2
toK '.' '.'  _   _  = 3
toK '.'  _  '.' '.' = 4
toK '.'  _  '.'  _  = 5
toK '.'  _   _  '.' = 6
toK '.'  _   _   _  = 7
toK  _  '.' '.' '.' = 8
toK  _  '.' '.'  _  = 9
toK  _  '.'  _  '.' = 10
toK  _  '.'  _   _  = 11
toK  _   _  '.' '.' = 12
toK  _   _  '.'  _  = 13
toK  _   _   _  '.' = 14
toK  _   _   _   _  = 15

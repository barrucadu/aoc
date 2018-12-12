module Common where

import qualified Data.IntSet as S

type Transitions = (S.IntSet, S.IntSet)

parse :: String -> ([Bool], Transitions)
{-# INLINABLE parse #-}
parse = go1 . lines where
  go1 ((_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:rest):[]:transitions) = (map (=='#') rest, go2 S.empty S.empty transitions)
  go1 _ = error "invalid input"

  go2 trdot trhash ([l1, l2, '.', r1, r2, _, _, _, _, '#']:rest) =
    go2 (S.insert (toK (l1=='#') (l2=='#') (r1=='#') (r2=='#')) trdot) trhash rest
  go2 trdot trhash ([l1, l2, '#', r1, r2, _, _, _, _, '#']:rest) =
    go2 trdot (S.insert (toK (l1=='#') (l2=='#') (r1=='#') (r2=='#')) trhash) rest
  go2 trdot trhash (_:rest) = go2 trdot trhash rest
  go2 trdot trhash [] = (trdot, trhash)

findTransition :: Transitions -> Bool -> Int -> Bool
{-# INLINABLE findTransition #-}
findTransition (_, s) True  k = k `S.member` s
findTransition (s, _) False k = k `S.member` s

toK :: Bool -> Bool -> Bool -> Bool -> Int
{-# INLINABLE toK #-}
toK False False False False = 0
toK False False False     _ = 1
toK False False     _ False = 2
toK False False     _     _ = 3
toK False     _ False False = 4
toK False     _ False     _ = 5
toK False     _     _ False = 6
toK False     _     _     _ = 7
toK     _ False False False = 8
toK     _ False False     _ = 9
toK     _ False     _ False = 10
toK     _ False     _     _ = 11
toK     _     _ False False = 12
toK     _     _ False     _ = 13
toK     _     _     _ False = 14
toK     _     _     _     _ = 15

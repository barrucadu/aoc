{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Common where

import Control.Arrow (first)
import Control.Monad (filterM)
import Control.Monad.ST (ST)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, listToMaybe, maybeToList)
import qualified Data.Set as S

import Utils

type Cell = Int
pattern COpen   <- 0 where COpen   = 0
pattern CWall   <- 1 where CWall   = 1
pattern CElf    <- 2 where CElf    = 2
pattern CGoblin <- 3 where CGoblin = 3

type Pos = (Int, Int)

data Victor = Elf | Goblin
  deriving (Eq, Show)

parse :: String -> ST s (STArray s Cell, M.Map Pos Int, M.Map Pos Int)
{-# INLINABLE parse #-}
parse input0 = do
    let ls = lines input0
    let height = length ls
    let width = length (head ls)
    arr <- newArray height width
    go arr ls
  where
    go arr = go' M.empty M.empty 0 0 where
      go' es gs _ _ [] = pure (arr, es, gs)
      go' es gs _ y ([]:ls) = go' es gs 0 (y+1) ls
      go' es gs x y (('.':l):ls) = do
        writeArray arr y x COpen
        go' es gs (x+1) y (l:ls)
      go' es gs x y (('#':l):ls) = do
        writeArray arr y x CWall
        go' es gs (x+1) y (l:ls)
      go' es gs x y (('E':l):ls) = do
        writeArray arr y x CElf
        go' (M.insert (y, x) elfHP es) gs (x+1) y (l:ls)
      go' es gs x y (('G':l):ls) = do
        writeArray arr y x CGoblin
        go' es (M.insert (y, x) gobHP gs) (x+1) y (l:ls)
      go' _ _ _ _ _ = error "invalid input"

    elfHP = 200
    gobHP = 200

battle :: Int -> Int -> STArray s Cell -> M.Map Pos Int -> M.Map Pos Int -> ST s (Int, Int, Int, Victor)
{-# INLINABLE battle #-}
battle elfAttk gobAttk arr = go 0 where
  go !n es gs
    | M.null es = pure (n, score gs, M.size gs, Goblin)
    | M.null gs = pure (n, score es, M.size es, Elf)
    | otherwise = do
        (ok, es', gs') <- turn elfAttk gobAttk arr es gs
        if | ok         -> go (n+1) es' gs'
           | M.null es' -> pure (n, score gs', M.size gs', Goblin)
           | otherwise  -> pure (n, score es', M.size es', Elf)

  score = sum . M.elems

turn :: Int -> Int -> STArray s Cell -> M.Map Pos Int -> M.Map Pos Int -> ST s (Bool, M.Map Pos Int, M.Map Pos Int)
{-# INLINABLE turn #-}
turn elfAttk gobAttk arr es0 gs0 = go es0 gs0 (M.keys es0) (M.keys gs0) where
  go es gs ee0@(e:ee) gg0@(g:gg)
    -- skip over killed units
    | e `M.notMember` es = go es gs ee gg0
    | g `M.notMember` gs = go es gs ee0 gg
    | e <= g    = elf    es gs e ee gg0
    | otherwise = goblin es gs g ee0 gg
  go es gs (e:ee) gg0
    | e `M.notMember` es = go es gs ee gg0
    | otherwise = elf es gs e ee gg0
  go es gs ee0 (g:gg)
    | g `M.notMember` gs = go es gs ee0 gg
    | otherwise = goblin es gs g ee0 gg
  go es gs [] [] = pure (True, es, gs)

  elf es gs me ee gg
    | M.null gs = pure (False, es, gs)
    | otherwise = step elfAttk es gs me >>= \(yx', es', gs') -> go es' gs' (kill yx' ee) (kill yx' gg)

  goblin es gs me ee gg
    | M.null es = pure (False, es, gs)
    | otherwise = step gobAttk gs es me >>= \(yx', gs', es') -> go es' gs' (kill yx' ee) (kill yx' gg)

  kill a = filter (/=a)

  step dam us them yx@(y, x) = case pickTarget yx them of
    Just target -> do
      them' <- attack dam them target
      pure (yx, us, them')
    Nothing -> do
      yx'@(y', x') <- move yx . concat =<< mapM neighbours (M.keys them)
      let hp = fromJust (M.lookup yx us)
      let us' = M.insert yx' hp (M.delete yx us)
      whoami <- readArray arr y x
      writeArray arr y x COpen
      writeArray arr y' x' whoami
      case pickTarget yx' them of
        Just target -> do
          them' <- attack dam them target
          pure (yx', us', them')
        Nothing -> pure (yx', us', them)

  pickTarget (y, x) them =
    let ps = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
        ns = [(hp, p) | p <- ps, hp <- maybeToList (M.lookup p them)]
    in listToMaybe (sort ns)

  attack dam them (thp, tyx@(ty, tx))
    | thp <= dam = do
        writeArray arr ty tx COpen
        pure (M.delete tyx them)
    | otherwise = pure (M.insert tyx (thp - dam) them)

  move (y0, x0) targets = do
      grid <- newArray (heightArray' arr) (widthArray' arr)
      setArray grid (inf, (inf, inf))
      flood grid (S.fromList [(t, (0, t)) | t <- targets])
      let r y x = (, (y, x)) <$> readArray grid y x
      points <- sequence
        [ r (y0 - 1) x0
        , r y0       (x0 - 1)
        , r y0       (x0 + 1)
        , r (y0 + 1) x0
        ]
      pure $ case sort points of
        (((bestD, _), yx):_)
          | bestD == inf -> (y0, x0)
          | otherwise    -> yx
        [] -> (y0, x0)
    where
      flood grid points
        | S.null points = pure ()
        | otherwise = do
          let ((yx@(y, x), val), points') = S.deleteFindMin points
          cur <- readArray grid y x
          if cur <= val
            then flood grid points'
            else do
              writeArray grid y x val
              ns <- neighbours yx
              flood grid (S.union points' (S.fromList [(n, first (+1) val) | n <- ns]))

  neighbours (y, x) = filterM unoccupied [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
  unoccupied (y, x) = (==COpen) <$> readArray arr y x

  -- a distance larger than anything reachable
  inf = widthArray' arr + heightArray' arr + 1

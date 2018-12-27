{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Monad (filterM)
import Control.Monad.ST (ST, runST)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as S

import Utils

type Cell = Int
pattern COpen   <- 0 where COpen   = 0
pattern CWall   <- 1 where CWall   = 1
pattern CElf    <- 2 where CElf    = 2
pattern CGoblin <- 3 where CGoblin = 3

type Pos = (Int, Int)

main :: IO ()
main = mainFor 15 id (show . solve)

parse :: String -> ST s (STArray s Cell, M.Map Pos Int, M.Map Pos Int)
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

solve :: String -> Int
solve input0 = runST $ do
  (arr, es, gs)  <- parse input0
  (turns, score) <- battle arr es gs
  pure (turns * score)

battle :: STArray s Cell -> M.Map Pos Int -> M.Map Pos Int -> ST s (Int, Int)
battle arr = go 0 where
  go !n es gs
    | M.null es = pure (n, score gs)
    | M.null gs = pure (n, score es)
    | otherwise = do
        (ok, es', gs') <- turn arr es gs
        if | ok         -> go (n+1) es' gs'
           | M.null es' -> pure (n, score gs')
           | otherwise  -> pure (n, score es')

  score = sum . M.elems

turn :: STArray s Cell -> M.Map Pos Int -> M.Map Pos Int -> ST s (Bool, M.Map Pos Int, M.Map Pos Int)
turn arr es0 gs0 = go es0 gs0 (M.toAscList es0) (M.toAscList gs0) where
  go es gs ee0@(e:ee) gg0@(g:gg)
    -- skip over killed units
    | fst e `M.notMember` es = go es gs ee gg0
    | fst g `M.notMember` gs = go es gs ee0 gg
    | e <= g    = elf    es gs e ee gg0
    | otherwise = goblin es gs g ee0 gg
  go es gs (e:ee) gg0
    | fst e `M.notMember` es = go es gs ee gg0
    | otherwise = elf es gs e ee gg0
  go es gs ee0 (g:gg)
    | fst g `M.notMember` gs = go es gs ee0 gg
    | otherwise = goblin es gs g ee0 gg
  go es gs [] [] = pure (True, es, gs)

  elf es gs me ee gg
    | M.null gs = pure (False, es, gs)
    | otherwise = step elfAttk es gs me >>= \(es', gs') -> go es' gs' ee gg

  goblin es gs me ee gg
    | M.null es = pure (False, es, gs)
    | otherwise = step gobAttk gs es me >>= \(gs', es') -> go es' gs' ee gg

  step dam us them (yx@(y, x), hp) = case pickTarget yx them of
    Just target -> do
      them' <- attack dam them target
      pure (us, them')
    Nothing -> do
      yx'@(y', x') <- move yx . concat =<< mapM neighbours (M.keys them)
      let us' = M.insert yx' hp (M.delete yx us)
      whoami <- readArray arr y x
      writeArray arr y x COpen
      writeArray arr y' x' whoami
      case pickTarget yx' them of
        Just target -> do
          them' <- attack dam them target
          pure (us', them')
        Nothing -> pure (us', them)

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
      setArray grid inf
      flood grid (S.fromList [(t, 0) | t <- targets])
      up    <- readArray grid (y0 - 1) x0
      left  <- readArray grid y0       (x0 - 1)
      right <- readArray grid y0       (x0 + 1)
      down  <- readArray grid (y0 + 1) x0
      let best = minimum [up, left, right, down]
      pure $
        if | best == inf   -> (y0, x0)
           | best == up    -> (y0 - 1, x0)
           | best == left  -> (y0, x0 - 1)
           | best == right -> (y0, x0 + 1)
           | otherwise     -> (y0 + 1, x0)
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
              flood grid (S.union points' (S.fromList [(n, val + 1) | n <- ns]))

  neighbours (y, x) = filterM unoccupied [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
  unoccupied (y, x) = (==COpen) <$> readArray arr y x

  elfAttk = 3
  gobAttk = 3

  -- a distance larger than anything reachable
  inf = widthArray' arr + heightArray' arr + 1

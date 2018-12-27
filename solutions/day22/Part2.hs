{-# LANGUAGE PatternSynonyms #-}

import           Control.Monad.ST (ST, runST)
import           Data.Foldable    (for_)
import           Data.Graph.AStar
import qualified Data.HashSet     as S
import           Data.Maybe       (fromJust)

import           Common
import           Utils

type Tool = Int
pattern T <- 0 where T = 0
pattern C <- 1 where C = 1
pattern N <- 2 where N = 2

main :: IO ()
main = mainFor 22 parse (show . solve)

solve :: (Int, (Int, Int)) -> Int
solve (depth, (tX, tY)) = pathLen path where
  maxX = tX * 100
  maxY = tY * 100

  path = runST $ do
    gIs <- newArray (maxX+1) (maxY+1)
    eLs <- newArray (maxX+1) (maxY+1)
    setGIEL gIs eLs 0  0  0
    for_ [1..maxX] $ \x -> setGIEL gIs eLs x 0 (x * 16807)
    for_ [1..maxY] $ \y -> setGIEL gIs eLs 0 y (y * 48271)
    for_ [1..tX] $ \x ->
      for_ [1..tY] $ \y -> calcGIEL gIs eLs x y
    setGIEL gIs eLs tX tY 0
    for_ [tX+1..maxX] $ \x ->
      for_ [1..tY] $ \y -> calcGIEL gIs eLs x y
    for_ [1..maxX] $ \x ->
      for_ [tY+1..maxY] $ \y -> calcGIEL gIs eLs x y
    findPath eLs

  setGIEL :: STArray s Int -> STArray s Int -> Int -> Int -> Int -> ST s ()
  setGIEL gIs eLs x y gi = do
    let el = (gi + depth) `mod` 20183
    writeArray gIs x y gi
    writeArray eLs x y el

  calcGIEL :: STArray s Int -> STArray s Int -> Int -> Int -> ST s ()
  calcGIEL gIs eLs x y = do
    left <- readArray eLs (x-1) y
    above <- readArray eLs x (y-1)
    let gi = left * above
    setGIEL gIs eLs x y gi

  findPath :: STArray s Int -> ST s [((Int, Int), Tool)]
  findPath eLs = fromJust <$> aStarM neighbours distance heuristic goal (pure t0) where
    neighbours p@((x, y), t) = do
      here  <- changeTool t <$> safeRead x y
      up    <- neighbour  t <$> safeRead x (y-1)
      down  <- neighbour  t <$> safeRead x (y+1)
      left  <- neighbour  t <$> safeRead (x-1) y
      right <- neighbour  t <$> safeRead (x+1) y

      pure $ S.fromList (here ++ up ++ down ++ left ++ right)

    safeRead x y
      | x < 0 || x > maxX = pure Nothing
      | y < 0 || y > maxY = pure Nothing
      | otherwise = do
          el <- readArray eLs x y
          pure $ Just ((x, y), el `mod` 3)

    changeTool C (Just (xy, 0)) = [(xy, T)]
    changeTool T (Just (xy, 0)) = [(xy, C)]
    changeTool C (Just (xy, 1)) = [(xy, N)]
    changeTool N (Just (xy, 1)) = [(xy, C)]
    changeTool T (Just (xy, 2)) = [(xy, N)]
    changeTool N (Just (xy, 2)) = [(xy, T)]
    changeTool _ _ = []

    neighbour C (Just (xy, 0)) = [(xy, C)]
    neighbour T (Just (xy, 0)) = [(xy, T)]
    neighbour C (Just (xy, 1)) = [(xy, C)]
    neighbour N (Just (xy, 1)) = [(xy, N)]
    neighbour T (Just (xy, 2)) = [(xy, T)]
    neighbour N (Just (xy, 2)) = [(xy, N)]
    neighbour _ _ = []

    distance a b = pure (nodeDist a b)

    heuristic = distance tP

    goal p = pure (p == tP)

    tP = ((tX, tY), T)
    t0 = ((0, 0), T)

  pathLen = go ((0, 0), T) where
    go c (n:ps) = nodeDist c n + go n ps
    go _ _ = 0

  nodeDist (xy1, t1) (xy2, t2)
    | t1 == t2  = manhattan2 xy1 xy2
    | otherwise = manhattan2 xy1 xy2 + 7

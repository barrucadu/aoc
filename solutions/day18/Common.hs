{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)

import Utils

type Cell = Int
pattern COpen <- 0 where COpen = 0
pattern CTrees <- 1 where CTrees = 1
pattern CLumberyard  <- 2 where CLumberyard = 2

type Automata s = STArray s Cell

type Dimensions = (Int, Int)

-- | Get the width and height of the input grid.
getWH :: String -> Dimensions
{-# INLINABLE getWH #-}
getWH input = (length (head ls), length ls) where
  ls = lines input

-- | Fling the input into an array.
parse :: Automata s -> String -> ST s ()
{-# INLINABLE parse #-}
parse s = go 0 0 where
  go _ _ [] = pure ()
  go _ y ('\n':cs) = go 0 (y+1) cs
  go x y (c:cs) = do
    writeArray s x y $ case c of
      '.' -> COpen
      '|' -> CTrees
      '#' -> CLumberyard
      _   -> error "invalid input"
    go (x+1) y cs

-- | Calculate the next generation.
step
  :: Dimensions
  -> Automata s -- ^ Source
  -> Automata s -- ^ Target
  -> ST s ()
{-# INLINABLE step #-}
step (width, height) sCur sNext =
    for_ [0..width-1]  $ \x ->
    for_ [0..height-1] $ \y ->
    writeArray sNext x y =<< rules' sCur x y
  where
    rules' s x y = do
      now <- readArray s x y
      ns  <- sequence [safeRead s x' y' | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]
      pure (rules now (catMaybes ns))

    safeRead s x y
      | x < 0 || x >= width  = pure Nothing
      | y < 0 || y >= height = pure Nothing
      | otherwise = Just <$> readArray s x y

-- | Work out what comes next, given the current value and the
-- neighbours.
rules :: Cell -> [Cell] -> Cell
{-# INLINABLE rules #-}
--- An open acre will become filled with trees if three or more
--- adjacent acres contained trees. Otherwise, nothing happens.
rules COpen ns
  | length (filter (==CTrees) ns) >= 3 = CTrees
  | otherwise = COpen
-- An acre filled with trees will become a lumberyard if three or more
-- adjacent acres were lumberyards. Otherwise, nothing happens.
rules CTrees ns
  | length (filter (==CLumberyard) ns) >= 3 = CLumberyard
  | otherwise = CTrees
-- An acre containing a lumberyard will remain a lumberyard if it was
-- adjacent to at least one other lumberyard and at least one acre
-- containing trees. Otherwise, it becomes open.
rules CLumberyard ns
  | null (filter (==CLumberyard) ns) = COpen
  | null (filter (==CTrees) ns) = COpen
  | otherwise = CLumberyard
rules c _ = c

-- | Work out the score.
score :: Dimensions -> Automata s -> ST s Int
score (width, height) s = go 0 0 0 0 where
  go !accT !accL x y
    | y == height = pure (accT * accL)
    | x == width  = go accT accL 0 (y+1)
    | otherwise = readArray s x y >>= \case
        CTrees      -> go (accT+1) accL (x+1) y
        CLumberyard -> go accT (accL+1) (x+1) y
        _ -> go accT accL (x+1) y

-- | Render to a string (for debugging)
render :: Dimensions -> Automata s -> ST s String
render (width, height) s = go 0 0 where
  go x y
    | y == height = pure []
    | x == width = ('\n':) <$> go 0 (y+1)
    | otherwise = readArray s x y >>= \case
        COpen -> ('.':) <$> go (x+1) y
        CTrees -> ('|':) <$> go (x+1) y
        CLumberyard -> ('#':) <$> go (x+1) y

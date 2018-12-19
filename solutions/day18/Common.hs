{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)

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
step (width, height) sCur sNext = do
    -- corners
    rulesTL
    rulesTR
    rulesBL
    rulesBR
    -- edges
    for_ [1..width-2] $ \x -> do
      rulesT x
      rulesB x
    for_ [1..height-2] $ \y -> do
      rulesL y
      rulesR y
    -- interior
    for_ [1..width-2] $ \x ->
      for_ [1..height-2] $ \y ->
        rules' x y
  where
    rulesTL = do
      now <- readArray sCur 0 0
      ns  <- sequence
        [ readArray sCur 1 0
        , readArray sCur 1 1
        , readArray sCur 0 1
        ]
      writeArray sNext 0 0 (rules now ns)

    rulesTR = do
      now <- readArray sCur (width - 1) 0
      ns  <- sequence
        [ readArray sCur (width - 2) 0
        , readArray sCur (width - 2) 1
        , readArray sCur (width - 1) 1
        ]
      writeArray sNext (width - 1) 0 (rules now ns)

    rulesBL = do
      now <- readArray sCur 0 (height - 1)
      ns  <- sequence
        [ readArray sCur 1 (height - 1)
        , readArray sCur 1 (height - 2)
        , readArray sCur 0 (height - 2)
        ]
      writeArray sNext 0 (height - 1) (rules now ns)

    rulesBR = do
      now <- readArray sCur (width - 1) (height - 1)
      ns  <- sequence
        [ readArray sCur (width - 2) (height - 1)
        , readArray sCur (width - 2) (height - 2)
        , readArray sCur (width - 1) (height - 2)
        ]
      writeArray sNext (width - 1) (height - 1) (rules now ns)

    rulesT x = do
      now <- readArray sCur x 0
      ns  <- sequence
        [ readArray sCur (x - 1) 0
        , readArray sCur (x + 1) 0
        , readArray sCur (x - 1) 1
        , readArray sCur x       1
        , readArray sCur (x + 1) 1
        ]
      writeArray sNext x 0 (rules now ns)

    rulesB x = do
      now <- readArray sCur x (height - 1)
      ns  <- sequence
        [ readArray sCur (x - 1) (height - 1)
        , readArray sCur (x + 1) (height - 1)
        , readArray sCur (x - 1) (height - 2)
        , readArray sCur x       (height - 2)
        , readArray sCur (x + 1) (height - 2)
        ]
      writeArray sNext x (height - 1) (rules now ns)

    rulesL y = do
      now <- readArray sCur 0 y
      ns  <- sequence
        [ readArray sCur 0 (y - 1)
        , readArray sCur 0 (y + 1)
        , readArray sCur 1 (y - 1)
        , readArray sCur 1 y
        , readArray sCur 1 (y + 1)
        ]
      writeArray sNext 0 y (rules now ns)

    rulesR y = do
      now <- readArray sCur (width - 1) y
      ns  <- sequence
        [ readArray sCur (width - 1) (y - 1)
        , readArray sCur (width - 1) (y + 1)
        , readArray sCur (width - 2) (y - 1)
        , readArray sCur (width - 2) y
        , readArray sCur (width - 2) (y + 1)
        ]
      writeArray sNext (width - 1) y (rules now ns)

    rules' x y = do
      now <- readArray sCur x y
      ns  <- sequence
        [ readArray sCur (x - 1) (y - 1)
        , readArray sCur (x - 1) y
        , readArray sCur (x - 1) (y + 1)
        , readArray sCur x       (y - 1)
        , readArray sCur x       (y + 1)
        , readArray sCur (x + 1) (y - 1)
        , readArray sCur (x + 1) y
        , readArray sCur (x + 1) (y + 1)
        ]
      writeArray sNext x y (rules now ns)

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

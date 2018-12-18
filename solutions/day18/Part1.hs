{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Monad.ST (ST, runST)

import Data.Foldable (for_)

import Utils

type Cell = Int
pattern COpen <- 0 where COpen = 0
pattern CTrees <- 1 where CTrees = 1
pattern CLumberyard  <- 2 where CLumberyard = 2

type Automata s = STArray s Cell

main :: IO ()
main = mainFor 18 id (show . solve)

solve :: String -> Int
solve input = runST $ do
    let (w, h) = let ls = lines input in (length (head ls), length ls)
    sCur  <- newArray w h
    sNext <- newArray w h
    parse sCur input
    scan w h =<< solve' w h 10 sCur sNext
  where
    parse :: STArray s Cell -> String -> ST s ()
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

    solve' :: Int -> Int -> Int -> STArray s Cell -> STArray s Cell -> ST s (STArray s Cell)
    solve' width height = go where
      go  0 sCur _ = pure sCur
      go !n sCur sNext = do
        step sCur sNext
        go (n-1) sNext sCur

      step sCur sNext =
        for_ [0..width-1] $ \x ->
          for_ [0..height-1] $ \y ->
            writeArray sNext x y =<< rules sCur x y

      rules s x y = do
        now <- readArray s x y
        ns  <- sequence [safeRead s x' y' | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]
        pure (rules' now ns)

      --- An open acre will become filled with trees if three or more
      --- adjacent acres contained trees. Otherwise, nothing happens.
      rules' COpen ns
        | length (filter (==Just CTrees) ns) >= 3 = CTrees
        | otherwise = COpen
      -- An acre filled with trees will become a lumberyard if three
      -- or more adjacent acres were lumberyards. Otherwise, nothing
      -- happens.
      rules' CTrees ns
        | length (filter (==Just CLumberyard) ns) >= 3 = CLumberyard
        | otherwise = CTrees
      -- An acre containing a lumberyard will remain a lumberyard if
      -- it was adjacent to at least one other lumberyard and at least
      -- one acre containing trees. Otherwise, it becomes open.
      rules' CLumberyard ns
        | null (filter (==Just CLumberyard) ns) = COpen
        | null (filter (==Just CTrees) ns) = COpen
        | otherwise = CLumberyard
      rules' c _ = c

      safeRead s x y
        | x < 0 || x >= width  = pure Nothing
        | y < 0 || y >= height = pure Nothing
        | otherwise = Just <$> readArray s x y

    scan :: Int -> Int -> STArray s Cell -> ST s Int
    scan width height s = go 0 0 0 0 where
      go !accT !accL x y
        | y == height = pure (accT * accL)
        | x == width  = go accT accL 0 (y+1)
        | otherwise = readArray s x y >>= \case
            CTrees      -> go (accT+1) accL (x+1) y
            CLumberyard -> go accT (accL+1) (x+1) y
            _ -> go accT accL (x+1) y

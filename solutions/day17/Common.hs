{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Foldable (for_)

import Utils

type SqM = Int
pattern SSand <- 0 where SSand = 0
pattern SClay <- 1 where SClay = 1
pattern SWaterFlowing  <- 2 where SWaterFlowing  = 2
pattern SWaterStanding <- 3 where SWaterStanding = 3

type Range = (Int, Int, Int, Int)

data Boundary = Wall Int Int | Spill Int

parse :: String -> [Range]
{-# INLINABLE parse #-}
parse input0 = map parseLine (lines input0) where
  parseLine ('x':'=':rest) =
    let (xlo, xhi, ' ':'y':'=':rest') = parseRange rest
        (ylo, yhi, _) = parseRange rest'
    in (xlo, xhi, ylo, yhi)
  parseLine ('y':'=':rest) =
    let (ylo, yhi, ' ':'x':'=':rest') = parseRange rest
        (xlo, xhi, _) = parseRange rest'
    in (xlo, xhi, ylo, yhi)
  parseLine _ = error "invalid input"

  parseRange rest = case parseN 0 rest of
    (lo, '.':rest') ->
      let (hi, rest'') = parseN 0 rest'
      in (lo, hi, rest'')
    (lo, rest') -> (lo, lo, rest')

  parseN !acc (',':rest) = (acc, rest)
  parseN !acc ('.':rest) = (acc, rest)
  parseN !acc (c:rest) = parseN (stepParseInt acc c) rest
  parseN !acc [] = (acc, [])

waterfall :: [Range] -> (Int, Int, Int, Int, Array SqM)
{-# INLINABLE waterfall #-}
waterfall ranges = (minX, maxX, minY, maxY, aout) where
  aout = createArray $ do
    arr <- newArray maxX (maxY+1)
    for_ ranges (draw arr)
    flow arr startX startY
    pure arr

  startX = 500
  startY = minY - 1

  minX = minimum (map (\(xlo, _, _, _) -> xlo) ranges)
  maxX = maximum (map (\(_, xhi, _, _) -> xhi) ranges) + 1
  minY = minimum (map (\(_, _, ylo, _) -> ylo) ranges)
  maxY = maximum (map (\(_, _, _, yhi) -> yhi) ranges)

  draw :: STArray s SqM -> Range -> ST s ()
  draw arr (xlo, xhi, ylo, yhi) =
    for_ [xlo..xhi] $ \x ->
      for_ [ylo..yhi] $ \y ->
        writeArray arr x y SClay

  flow arr = flow' where
    flow' x y
      | y > maxY = pure ()
      | otherwise = readArray arr x y >>= \case
        SSand -> do
          writeArray arr x y SWaterFlowing
          flow' x (y+1)
          readArray arr x (y+1) >>= \case
            SClay -> across x y
            SWaterStanding -> across x y
            _ -> pure ()
        _ -> pure ()

    across x y = check x y >>= \case
      (Wall SClay xL, Wall SClay xR) ->
        row SWaterStanding (xL+1) (xR-1) y
      (Wall _     xL, Wall _     xR) ->
        row SWaterFlowing  (xL+1) (xR-1) y
      (Wall _     xL, Spill      xR) -> do
        row SWaterFlowing (xL+1) xR y
        spill xR y
      (Spill      xL, Wall _     xR) -> do
        row SWaterFlowing xL (xR-1) y
        spill xL y
      (Spill      xL, Spill      xR) -> do
        row SWaterFlowing xL xR y
        spill xL y
        spill xR y
        cL <- readArray arr xL (y+1)
        cR <- readArray arr xR (y+1)
        when (cL == SWaterStanding && cR == SWaterStanding) $
          across x y

    check x0 y = (,) <$> check' pred x0 <*> check' succ x0 where
      check' f !x = readArray arr x (y+1) >>= \case
        SSand -> pure (Spill x)
        SWaterFlowing -> pure (Wall SWaterFlowing x)
        _ -> readArray arr x y >>= \case
          SClay -> pure (Wall SClay x)
          _ -> check' f (f x)

    row water xL xR y = for_ [xL..xR] $
      \x -> writeArray arr x y water

    spill x y = do
      flow' x (y+1)
      readArray arr x (y+1) >>= \case
        SWaterStanding -> across x y
        _ -> pure ()

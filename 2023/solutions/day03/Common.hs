{-# LANGUAGE BangPatterns #-}

module Common where

import           Data.Char (isDigit)
import qualified Data.Set  as S

import           Utils

type Point = (Int, Int)
data Token = Token
  { tValue :: Int
  , tPos :: Point
  , tWidth :: Int
  , tNeighbours :: S.Set Point
  }
  deriving Show

parse :: (Char -> Bool) -> String -> ([Token], S.Set Point)
parse checkPart = go 0 [] S.empty . lines where
  go !y tokens parts (l:ls) = goLine 0 y tokens parts l ls
  go !_ tokens parts [] = (tokens, parts)

  goLine !_ !y tokens parts [] ls = go (y+1) tokens parts ls
  goLine !x !y tokens parts ('.':cs) ls = goLine (x+1) y tokens parts cs ls
  goLine !x !y tokens parts (c:cs) ls
    | isDigit c =
      let (token, cs') = goToken (x, y) 1 (parseDigit c) cs
          x' = x + tWidth token
          tokens' = token : tokens
      in goLine x' y tokens' parts cs' ls
    | otherwise =
      let parts' = if checkPart c then S.insert (x, y) parts else parts
      in goLine (x+1) y tokens parts' cs ls

  goToken xy !w !acc cs0@(c:cs)
    | isDigit c = goToken xy (w+1) (stepParseInt acc c) cs
    | otherwise = (toToken acc xy w, cs0)
  goToken xy !w !acc [] = (toToken acc xy w, [])

toToken :: Int -> Point -> Int -> Token
toToken n xy@(x0, y0) w = Token n xy w neighbours where
  neighbours = S.fromList $ leftright ++ updown where
    leftright = [(x0 + dx, y0 + dy) | dx <- [-1, w], dy <- [-1, 0, 1]]
    updown = [(x0 + dx, y0 + dy) | dx <- [0..w], dy <- [-1, 1]]

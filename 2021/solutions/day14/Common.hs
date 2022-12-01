module Common where

import qualified Data.Map as M

import           Utils    (minmax)

type LSystemRules = M.Map (Char, Char) Char
type Polymer = M.Map (Char, Char) Int

parse :: String -> (LSystemRules, Polymer)
parse = go . lines where
  go ((c:cs):[]:rules) = (foldr parseRule M.empty rules, parsePolymer M.empty c cs)
  go _ = error "invalid input"

  parsePolymer m _ [] = m
  parsePolymer m a (b:bs) =
    let m' = M.insertWith (+) (a, b) 1 m
    in parsePolymer m' b bs

  parseRule [a, b, ' ', '-', '>', ' ', c] = M.insert (a, b) c
  parseRule _ = error "invalid input"

solve :: Int -> (LSystemRules, Polymer) -> Int
solve steps (rules, p0) = hi - lo where
  (lo, hi) = minmax . counts $ run steps p0

  run :: Int -> Polymer -> Polymer
  run 0 s = s
  run n s = run (n-1) (step s)

  step :: Polymer -> Polymer
  step = M.foldrWithKey apply M.empty where
    apply (a, b) n = case M.lookup (a, b) rules of
      Just o -> M.insertWith (+) (a, o) n . M.insertWith (+) (o, b) n
      Nothing -> M.insertWith (+) (a, b) n

  counts :: Polymer -> [Int]
  counts = map (\n -> (n + 1) `div` 2) . M.elems . M.foldrWithKey apply M.empty where
    apply (a, b) n = M.insertWith (+) a n . M.insertWith (+) b n

{-# LANGUAGE BangPatterns #-}

import Control.Arrow (first)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Utils

main :: IO ()
main = mainFor 25 parse (show . solve)

type P4 = (Int, Int, Int, Int)

parse :: String -> [P4]
parse = map go . lines where
  go l =
    let (x, rest) = goI l
        (y, rest') = goI rest
        (z, rest'') = goI rest'
        (t, []) = goI rest''
    in (x, y, z, t)

  goI ('-':cs) = first negate (goI' 0 cs)
  goI cs = goI' 0 cs

  goI' !acc [] = (fromIntegral acc, [])
  goI' !acc (',':cs) = (fromIntegral acc, cs)
  goI' !acc (c:cs) = goI' (stepParseInt acc c) cs

solve :: [P4] -> Int
solve = M.size . go M.empty M.empty where
  go rm m (p:ps) =
    let neighbours = filter (\p' -> manhattan4 p p' <= 3) ps
        (rm', m') = insert rm m p neighbours
    in go rm' m' ps
  go _ m [] = m

  insert :: Ord k => M.Map k k -> M.Map k (S.Set k) -> k -> [k] -> (M.Map k k, M.Map k (S.Set k))
  insert rm m p neighbours =
    let (rep:reps) = allReps p neighbours rm
        (rm', m') = merge rep rm m reps
        rm'' = setRep rep rm' (p:neighbours)
        m''  = connect rep (S.fromList (p:neighbours)) m'
    in (rm'', m'')

  merge :: Ord k => k -> M.Map k k -> M.Map k (S.Set k) -> [k] -> (M.Map k k, M.Map k (S.Set k))
  merge r0 rm m (r:rs) =
    let others = M.findWithDefault S.empty r m
        rm' = setRep r0 rm others
        m'  = connect r0 others (M.delete r m)
    in merge r0 rm' m' rs
  merge _ rm m [] = (rm, m)

  allReps :: Ord k => k -> [k] -> M.Map k k -> [k]
  allReps p neighbours rm = case mapMaybe (`M.lookup` rm) (p:neighbours) of
    [] -> [p]
    rs -> nub rs

  setRep :: (Foldable f, Ord k) => k -> M.Map k k -> f k -> M.Map k k
  setRep rep = foldr (`M.insert` rep)

  connect :: Ord k => k -> S.Set k -> M.Map k (S.Set k) -> M.Map k (S.Set k)
  connect = M.insertWith S.union

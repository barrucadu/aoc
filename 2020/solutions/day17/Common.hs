{-# LANGUAGE BangPatterns #-}

module Common where

import           Data.List       (iterate')

import qualified Data.Map.Strict as M

data State = Active | Inactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

genericParse :: Ord k => (Int -> Int -> k) -> String -> M.Map k State
genericParse toKey = goY 0 M.empty . lines where
  goY !y m (ls:lss) =
    let m' = goX y 0 m ls
    in goY (y+1) m' lss
  goY !_ m [] = m

  goX !y !x m ('#':cs) =
    let m' = M.insert (toKey y x) Active m
    in goX y (x+1) m' cs
  goX !y !x m (_:cs) =
    let m' = M.insert (toKey y x) Inactive m
    in goX y (x+1) m' cs
  goX !_ !_ m [] = m

genericSolve :: Ord k => Int -> M.Map k [k] -> M.Map k State -> Int
genericSolve sn neighbours m0 = countActive . (!!sn) $ iterate' step m0 where
  countActive = M.size . M.filter (==Active)

  step m = M.intersectionWith rule neighbours m where
    rule nearby state =
      let numActive = length $ filter (isActive m) nearby
      in case state of
           Active   | numActive `notElem` [2, 3] -> Inactive
           Inactive | numActive == 3 -> Active
           _ -> state

  isActive m wzyx = M.findWithDefault Inactive wzyx m == Active

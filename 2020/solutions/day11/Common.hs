{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.Map.Strict as M

import Utils (fixEq)

data Point = P {-# UNPACK#-} !Int {-# UNPACK#-} !Int
  deriving (Eq, Ord, Read, Show)

data State = Empty | Occupied
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

parse :: String -> M.Map Point State
parse = goY 0 M.empty . lines where
  goY !y m (ls:lss) =
    let m' = goX y 0 m ls
    in goY (y+1) m' lss
  goY !_ m [] = m

  goX !y !x m ('L':cs) =
    let m' = M.insert (P y x) Empty m
    in goX y (x+1) m' cs
  goX !y !x m (_:cs) = goX y (x+1) m cs
  goX !_ !_ m [] = m

genericSolve :: Int -> M.Map Point [Point] -> M.Map Point State -> Int
genericSolve threshold neighbours m0 = countOccupied $ fixEq step m0 where
  countOccupied :: M.Map Point State -> Int
  countOccupied = M.size . M.filter (==Occupied)

  step :: M.Map Point State -> M.Map Point State
  step m = M.intersectionWith rule neighbours m where
    rule :: [Point] -> State -> State
    rule nearby state =
      let numOccupied = length $ filter (isOccupied m) nearby
      in case state of
           Empty | numOccupied == 0 -> Occupied
           Occupied | numOccupied >= threshold -> Empty
           _ -> state

  isOccupied :: M.Map Point State -> Point -> Bool
  isOccupied m yx = M.findWithDefault Empty yx m == Occupied

dyxs :: [Point -> Point]
dyxs =
  [ \(P y x) -> P (y-1) (x-1)
  , \(P y x) -> P (y-1) x
  , \(P y x) -> P (y-1) (x+1)
  , \(P y x) -> P y (x-1)
  , \(P y x) -> P y (x+1)
  , \(P y x) -> P (y+1) (x-1)
  , \(P y x) -> P (y+1) x
  , \(P y x) -> P (y+1) (x+1)
  ]

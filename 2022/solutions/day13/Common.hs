{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils (stepParseInt)

data Atom = AInt Int | AList [Atom]
  deriving (Eq, Show)

instance Ord Atom where
  compare (AInt l) (AInt r) = compare l r
  compare l@(AInt _) (AList rs) = compare [l] rs
  compare (AList ls) r@(AInt _) = compare ls [r]
  compare (AList ls) (AList rs) = compare ls rs

type Packet = [Atom]

parse :: String -> [(Packet, Packet)]
parse = go . lines where
  go [] = []
  go ([]:ls) = go ls
  go (l1:l2:rest) = (parsePacket l1, parsePacket l2) : go rest

  parsePacket xs = let (AList as, []) = parseAtom xs in as

  parseAtom ('[':xs) = parseAList [] xs
  parseAtom (x:xs) = parseAInt (stepParseInt 0 x) xs

  parseAList acc (',':xs) = parseAList acc xs
  parseAList acc (']':xs) = (AList $ reverse acc, xs)
  parseAList acc xs = let (atom, rest) = parseAtom xs in parseAList (atom:acc) rest

  parseAInt !acc xs@(',':_) = (AInt acc, xs)
  parseAInt !acc xs@(']':_) = (AInt acc, xs)
  parseAInt !acc (x:xs) = parseAInt (stepParseInt acc x) xs

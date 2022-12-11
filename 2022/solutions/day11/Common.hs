{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.IntMap.Strict as M

import           Utils              (parseInt, stepParseInt)

data Monkey = Monkey
  { mInspects :: Int
  , mState :: [Integer]
  , mOp :: Integer -> Integer
  , mMod :: Integer
  , mTrue :: Int
  , mFalse :: Int
  }

parse :: String -> M.IntMap Monkey
parse = parseMonkey M.empty 0 . lines where
  parseMonkey ms k ([]:rest) = parseMonkey ms k rest
  parseMonkey ms k (_:l1:l2:l3:l4:l5:rest) =
    let state = parseState l1
        op = parseOp l2
        testMod = toInteger . parseInt . last . words $ l3
        testTrue = parseInt . last . words $ l4
        testFalse = parseInt . last . words $ l5
        m = Monkey { mInspects = 0, mState = state, mOp = op, mMod = testMod, mTrue = testTrue, mFalse = testFalse }
    in parseMonkey (M.insert k m ms) (k+1) rest
  parseMonkey ms _ _ = ms

  parseState (_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:l) = go [] 0 l where
    go acc !n [] = reverse (map toInteger (n:acc))
    go acc !n (',':' ':cs) = go (n:acc) 0 cs
    go acc !n (c:cs) = go acc (stepParseInt n c) cs

  parseOp (_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:op:_:l) =
    let fop = if op == '+' then (+) else (*)
    in if l == "old"
       then (\old -> old `fop` old)
       else (\old -> old `fop` toInteger (parseInt l))

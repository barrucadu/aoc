{-# LANGUAGE BangPatterns #-}

module Day4Common where

import Data.List (sort)

import Utils

-- | Parser for day 4, shared between both parts
parse :: String -> [(Int, [(Int, Int)], Int)]
{-# INLINABLE parse #-}
parse = go0 . sort . lines where
  go0 (('[':_:_:_:_:'-':_:_:'-':_:_:' ':_:_:':':_:_:']':' ':'G':'u':'a':'r':'d':' ':'#':rest):rest') = go1 0 rest rest'
  go0 [] = []
  go0 _ = error "malformed input"

  go1 acc " begins shift" rest' = go2 acc rest'
  go1 acc (d:rest) rest' = go1 (stepParseInt acc d) rest rest'
  go1 _ _ _ = error "malformed input"

  go2 guardnum xs0 = let (times, count, rest) = fallsAsleep [] 0 xs0 in (guardnum, times, count) : go0 rest where
    fallsAsleep times !count (('[':_:_:_:_:'-':_:_:'-':_:_:' ':'0':'0':':':m1:m2:"] falls asleep"):rest) =
      wakesUp times count (parseMinute m1 m2) rest
    fallsAsleep times !count rest = (times, count, rest)

    wakesUp times !count sleepTime (('[':_:_:_:_:'-':_:_:'-':_:_:' ':'0':'0':':':m1:m2:"] wakes up"):rest) =
      let wakeTime = parseMinute m1 m2
      in fallsAsleep ((sleepTime, wakeTime) : times) (count + wakeTime - sleepTime) rest
    wakesUp _ _ _ _ = error "malformed input"

    parseMinute m1 = stepParseInt (stepParseInt 0 m1)

module Day4Common where

import Data.List (sort)

import Utils

-- | Parser for day 4, shared between both parts
parse :: String -> [(Int, [(Int, Int)], Int)]
{-# INLINABLE parse #-}
parse = go0 . unlines . sort . lines where
  go0 ('[':_:_:_:_:'-':_:_:'-':_:_:' ':_:_:':':_:_:']':' ':'G':'u':'a':'r':'d':' ':'#':rest) = go1 0 rest
  go0 [] = []
  go0 _ = error "malformed input"

  go1 acc (' ':'b':'e':'g':'i':'n':'s':' ':'s':'h':'i':'f':'t':'\n':rest) = go2 acc rest
  go1 acc (d:rest) = go1 (stepParseInt acc d) rest
  go1 _ _ = error "malformed input"

  go2 guardnum xs0 = let (times, rest) = fallsAsleep [] xs0 in (guardnum, times, count times) : go0 rest where
    fallsAsleep times ('[':_:_:_:_:'-':_:_:'-':_:_:' ':'0':'0':':':m1:m2:']':' ':'f':'a':'l':'l':'s':' ':'a':'s':'l':'e':'e':'p':'\n':rest) =
      wakesUp times (parseMinute m1 m2) rest
    fallsAsleep times rest = (reverse times, rest)

    wakesUp times sleepTime ('[':_:_:_:_:'-':_:_:'-':_:_:' ':'0':'0':':':m1:m2:']':' ':'w':'a':'k':'e':'s':' ':'u':'p':'\n':rest) =
      fallsAsleep ((sleepTime, parseMinute m1 m2) : times) rest
    wakesUp _ _ _ = error "malformed input"

    parseMinute m1 = stepParseInt (stepParseInt 0 m1)

    count = sum . map (\(start, end) -> end - start)

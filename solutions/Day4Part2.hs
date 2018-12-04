{-# LANGUAGE LambdaCase #-}

import qualified Data.IntMap.Strict as M
import Data.List (sort)

import Utils

main :: IO ()
main = do
  input <- parse <$> readFile "../inputs/day4.txt"
  print (solve input)

parse :: String -> [(Int, [(Int, Int)], Int)]
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

solve :: [(Int, [(Int, Int)], Int)] -> Int
solve input = bestGuardNum * bestMinute where
  allTimes :: M.IntMap [[(Int,Int)]]
  allTimes = go M.empty input where
    go acc ((guardnum, times, _):rest) = case M.lookup guardnum acc of
      Just alltimes -> go (M.insert guardnum (times : alltimes) acc) rest
      Nothing -> go (M.insert guardnum [times] acc) rest
    go acc _ = acc

  (_, (bestGuardNum, bestMinute)) = maximum
      [ (go 0 minute (concat times), (guardnum, minute))
      | (guardnum, times) <- M.assocs allTimes
      , minute <- [0..59]
      ]
    where
      go acc minute ((start,end):rest)
        | minute >= start && minute < end = go (1 + acc) minute rest
        | otherwise = go acc minute rest
      go acc _ [] = acc

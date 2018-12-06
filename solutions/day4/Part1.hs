{-# LANGUAGE LambdaCase #-}

import qualified Data.IntMap.Strict as M

import Common

main :: IO ()
main = do
  input <- parse <$> readFile "../inputs/day4.txt"
  print (solve input)

solve :: [(Int, [(Int, Int)], Int)] -> Int
solve input = bestGuardNum * bestMinute where
  (bestGuardNum, bestTimes) = go 0 [] 0 M.empty input where
    go :: Int -> [[(Int,Int)]] -> Int -> M.IntMap ([[(Int,Int)]], Int) -> [(Int, [(Int, Int)], Int)] -> (Int, [(Int,Int)])
    go guardnum0 times0 total0 acc ((guardnum, times, total):rest) = case M.lookup guardnum acc of
      Just (alltimes, bigtotal) ->
        let total' = total + bigtotal
            times' = times : alltimes
            acc' = M.insert guardnum (times', total') acc
        in if total' > total0
           then go guardnum times' total' acc' rest
           else go guardnum0 times0 total0 acc' rest
      Nothing ->
        let times' = [times]
            acc' = M.insert guardnum (times', total) acc
        in if total > total0
           then go guardnum times' total acc' rest
           else go guardnum0 times0 total0 acc' rest
    go guardnum0 times0 _ _ _ = (guardnum0, concat times0)

  (_, bestMinute) = maximum [(go 0 minute bestTimes, minute) | minute <- [0..59]] where
    go :: Int -> Int -> [(Int, Int)] -> Int
    go acc minute ((start,end):rest)
      | minute >= start && minute < end = go (1 + acc) minute rest
      | otherwise = go acc minute rest
    go acc _ [] = acc

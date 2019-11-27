import qualified Data.IntMap.Strict as M

import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

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
      go :: Int -> Int -> [(Int, Int)] -> Int
      go acc minute ((start,end):rest)
        | minute >= start && minute < end = go (1 + acc) minute rest
        | otherwise = go acc minute rest
      go acc _ [] = acc

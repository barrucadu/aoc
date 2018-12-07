{-# LANGUAGE BangPatterns #-}

import Control.Arrow (first)
import Data.Char (ord)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: M.Map Char (Int, S.Set Char) -> Int
solve graph0 = go 0 5 [] initials graph0 where
  go :: Int -> Int -> [(Int, Char)] -> S.Set Char -> M.Map Char (Int, S.Set Char) -> Int
  go !n !idlers workers empties g
    | S.null empties && null workers = n - 1
    | otherwise =
      let workers' = map tick workers
          ((empties', g'), workers'', idlers') = foldr done ((empties, g), [], idlers) workers'
          (empties'', workers''', idlers'') = claim empties' workers'' idlers'
      in go (n + 1) idlers'' workers''' empties'' g'

  tick = first (subtract 1)

  done (0, c) ((empties, g), workers, idlers) = (deleteNode c empties g, workers, idlers+1)
  done w (eg, workers, idlers) = (eg, w:workers, idlers)

  claim empties workers 0 = (empties, workers, 0)
  claim empties workers !idlers
    | S.size empties > 0 =
      let smallest = S.elemAt 0 empties
          empties' = S.deleteAt 0 empties
      in claim empties' ((duration smallest, smallest):workers) (idlers-1)
    | otherwise = (empties, workers, idlers)

  deleteNode n empties g =
    let (_, ms) = M.findWithDefault (0, S.empty) n g
        g' = M.delete n g
        (empties', g'') = foldr checkAndRemove (empties, g') ms
    in (empties', g'')

  checkAndRemove m acc@(empties, g) = case M.lookup m g of
    Just (1, ms) -> (S.insert m empties, M.insert m (0, ms) g)
    Just (x, ms) -> (empties, M.insert m (x-1, ms) g)
    Nothing -> acc

  initials = M.keysSet (M.filter ((==0) . fst) graph0)

  duration c = 61 + ord c - ord 'A'

{-# LANGUAGE BangPatterns #-}

import Data.Char (ord)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: M.Map Char (Int, S.Set Char) -> Int
solve graph0 = go 0 workers0 initials graph0 where
  go !n ws empties g
    | S.null empties && all ((<0) . fst) ws = n - 1
    | otherwise =
      let ws' = foldr tick [] ws
          ((empties', g'), ws'') = foldr done ((empties, g), []) ws'
          (empties'', ws''') = foldr claim (empties', []) ws''
      in go (n+1) ws''' empties'' g'

  tick (-1, _) ws = (-1, '.'):ws
  tick (n, c)  ws = (n-1, c):ws

  done (0, c) ((empties, g), ws) = (deleteNode c empties g, (-1, c):ws)
  done w (eg, ws) = (eg, w:ws)

  claim w@(-1, _) (empties, ws)
    | S.size empties > 0 =
      let smallest = S.elemAt 0 empties
          empties' = S.deleteAt 0 empties
      in (empties', (duration smallest, smallest):ws)
    | otherwise = (empties, w:ws)
  claim w (empties, ws) = (empties, w:ws)

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

  workers0 = replicate 5 (1, '.')

  duration c = 61 + ord c - ord 'A'

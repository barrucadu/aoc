import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Common
import           Utils

main :: IO ()
main = mainFor 7 parse solve

solve :: M.Map Char (Int, S.Set Char) -> String
solve graph0 = reverse (go [] initials graph0) where
  go out empties g
    | S.null empties = out
    | otherwise =
      let smallest = S.elemAt 0 empties
          out' = smallest : out
          (empties', g') = deleteNode smallest (S.deleteAt 0 empties) g
      in go out' empties' g'

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

import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: CaveGraph -> Int
solve graph = go (S.singleton Start) Start where
  -- assuming there are no cycles of big caves, as the question says
  -- (1) to find all paths and (2) a big cave can be visited any
  -- number of times
  go _ End = 1
  go visitedSmall c@(Small _) = go' (S.insert c visitedSmall) c
  go visitedSmall c = go' visitedSmall c

  go' visitedSmall c = case M.lookup c graph of
    Just exits ->
      let exits' = S.toList $ exits `S.difference` visitedSmall
      in sum $ map (go visitedSmall) exits'
    Nothing -> 0

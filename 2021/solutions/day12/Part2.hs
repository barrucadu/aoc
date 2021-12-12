import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: CaveGraph -> Int
solve graph = go False S.empty Start where
  go _ _ End = 1
  go twice visitedSmall c@(Small _) = go' twice (S.insert c visitedSmall) c
  go twice visitedSmall c = go' twice visitedSmall c

  go' twice visitedSmall c = case M.lookup c graph of
    Just exits ->
      let notYetVisited = S.toList $ exits `S.difference` visitedSmall
          alreadyVisited = if twice then [] else S.toList $ exits `S.intersection` visitedSmall
      in sum (map (go twice visitedSmall) notYetVisited) + sum (map (go True visitedSmall) alreadyVisited)
    Nothing -> 0

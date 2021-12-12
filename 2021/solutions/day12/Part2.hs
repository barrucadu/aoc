import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

data OkSmall = Unvisited Cave | VisitedOnce Cave
  deriving (Eq, Ord, Read, Show)

solve :: CaveGraph -> Int
solve graph = sum [go okSmall (S.singleton Start) Start | okSmall <- Nothing : [Just (Unvisited c) | c@(Small _) <- M.keys graph]] where
  go Nothing _ End = 1
  go _ _ End = 0
  go okSmall visitedSmall c@(Small _)
    | okSmall == Just (Unvisited c) = go' (Just (VisitedOnce c)) visitedSmall c
    | okSmall == Just (VisitedOnce c) = go' Nothing (S.insert c visitedSmall) c
    | otherwise = go' okSmall (S.insert c visitedSmall) c
  go okSmall visitedSmall c = go' okSmall visitedSmall c

  go' okSmall visitedSmall c = case M.lookup c graph of
    Just exits ->
      let exits' = S.toList $ exits `S.difference` visitedSmall
      in sum $ map (go okSmall visitedSmall) exits'
    Nothing -> 0

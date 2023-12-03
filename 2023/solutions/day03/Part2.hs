import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 3 (parse (=='*')) (show . solve)

solve :: ([Token], S.Set Point) -> Int
solve (tokens, points) = sum [tValue a * tValue b | [a,b] <- M.elems gears] where
  gears = M.filter (\ts -> length ts == 2) $ M.fromSet getAdjacentTokens points
  getAdjacentTokens p = filter (S.member p . tNeighbours) tokens

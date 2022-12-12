import qualified Data.Graph.Inductive                   as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import qualified Data.Map.Strict                        as M
import           Data.Maybe                             (maybeToList)

import           Common
import           Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

parse :: String -> (G.Gr () Int, [G.Node], G.Node)
parse input =
  let width = widthOf input
      (grid, _, end) = toGrid input
      starts = [toNode width n | (n, c) <- M.assocs grid, c == 'a']
      graph = toGraph width grid
  in (graph, starts, toNode width end)

solve :: (G.Gr () Int, [G.Node], G.Node) -> Int
solve (graph, starts, end) = minimum [len | start <- starts, len <- maybeToList (G.getDistance start dijkstra)] where
  dijkstra = G.spTree end graph

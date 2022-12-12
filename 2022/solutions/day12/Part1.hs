import qualified Data.Graph.Inductive as G
import           Data.Maybe           (fromJust)

import           Common
import           Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

parse :: String -> (G.Gr () Int, G.Node, G.Node)
parse input =
  let width = widthOf input
      (grid, start, end) = toGrid input
      graph = toGraph width grid
  in (graph, toNode width start, toNode width end)

solve :: (G.Gr () Int, G.Node, G.Node) -> Int
solve (graph, start, end) = fromJust $ G.spLength end start graph

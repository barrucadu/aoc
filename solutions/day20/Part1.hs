import qualified Data.Graph.Inductive as G

import           Common
import           Utils

main :: IO ()
main = mainFor 20 parse (show . solve)

solve :: G.UGr -> Int
solve = maximum . map snd . G.level 0

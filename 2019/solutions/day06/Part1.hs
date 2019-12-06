import qualified Data.Map.Lazy as M

import           Common
import           Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

solve :: M.Map String (a, Int) -> Int
solve = sum . map snd . M.elems

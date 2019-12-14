import qualified Data.Map as M

import           Common
import           Utils

main :: IO ()
main = mainFor 14 parse (show . solve)

solve :: M.Map Ingredient Recipe -> Int
solve recipes = makeFuel recipes 1

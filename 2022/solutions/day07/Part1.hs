import qualified Data.Map as M

import           Common
import           Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: Filesystem -> Int
solve fs = sum [ size | size <- M.elems (directorySizes fs), size <= 100000 ]

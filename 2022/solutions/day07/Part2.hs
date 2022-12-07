import qualified Data.Map as M

import           Common
import           Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: Filesystem -> Int
solve fs = minimum [ size | size <- M.elems sizes, availableSpace + size >= neededSpace ] where
  availableSpace = 70000000 - sizes M.! []
  neededSpace = 30000000

  sizes = directorySizes fs

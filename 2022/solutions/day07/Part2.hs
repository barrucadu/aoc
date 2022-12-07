import qualified Data.Map as M

import           Common
import           Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: Filesystem -> Int
solve fs = minimum directorySizes where
  availableSpace = 70000000 - totalSize fs []
  neededSpace = 30000000

  directorySizes = [ size | dirname <- M.keys fs, let size = totalSize fs dirname, availableSpace + size >= neededSpace ]

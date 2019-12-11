import qualified Data.Map.Strict as M

import           Common
import           Intcode
import           Utils

main :: IO ()
main = mainFor 11 parse (show . solve)

solve :: Program -> Int
solve = M.size . runPainter 0

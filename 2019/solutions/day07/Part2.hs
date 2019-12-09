import           Data.List (foldl', permutations)

import           Common
import           Intcode   (Program, parse)
import           Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: Program -> Int
solve program = go (permutations [5,6,7,8,9]) where
  go = foldl' (\best phase -> max (go' phase) best) 0

  go' [pA,pB,pC,pD,pE] = runNetwork program pA pB pC pD pE
  go' _ = error "invalid phase specification"

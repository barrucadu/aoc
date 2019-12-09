import           Data.List (permutations)

import           Common
import           Intcode   (Program, parse)
import           Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: Program -> Int
solve program = go 0 (permutations [0,1,2,3,4]) where
  go best [] = best
  go best (phase:phases) =
    let candidate = go' phase
    in go (max candidate best) phases

  go' [pA,pB,pC,pD,pE] = runNetwork program pA pB pC pD pE
  go' _ = error "invalid phase specification"

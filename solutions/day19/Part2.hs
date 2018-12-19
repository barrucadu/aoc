import Control.Monad.ST (runST)

import Common
import Utils

main :: IO ()
main = mainFor 19 parse (show . solve)

solve :: (Int, Program) -> Int
solve (ipR, program) = runST $ do
  regs <- newRegs 1 0 0 0 0 0
  runProgram program ipR regs
  getReg regs 0

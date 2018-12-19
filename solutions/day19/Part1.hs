import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as V

import Common
import Utils

main :: IO ()
main = mainFor 19 parse (show . solve)

solve :: (Int, Program) -> Int
solve (ipR, program) = runST $ go 0 =<< newRegs 0 0 0 0 0 0 where
  go ip regs = do
    let instr = V.unsafeIndex program ip
    setReg regs ipR ip
    action instr regs
    ip' <- (+1) <$> getReg regs ipR
    if ip' >= V.length program
      then getReg regs 0
      else go ip' regs

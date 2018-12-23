import Control.Monad.ST (runST)
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as V

import Common
import Utils

main :: IO ()
main = mainFor 21 parse (show . solve)

solve :: (Int, Program) -> Int
solve (ipR, program) = runST $ do
    regs <- newRegs 0 0 0 0 0 0
    go regs S.empty 0 6
  where
    go regs seen prior 28 = do
      r5 <- getReg regs 5
      if r5 `S.member` seen
        then pure prior
        else go regs (S.insert r5 seen) r5 6
    go regs seen prior ip = do
      let instr = V.unsafeIndex program ip
      setReg regs ipR ip
      action instr regs
      ip' <- (+1) <$> getReg regs ipR
      go regs seen prior ip'

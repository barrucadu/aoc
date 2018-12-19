import qualified Data.Vector.Unboxed as V

import Common
import Utils

main :: IO ()
main = mainFor 19 parse (show . solve)

solve :: (Int, Program) -> Int
solve (ipR, program) = go 0 (0, 0, 0, 0, 0, 0) where
  go ip regs =
    let regs' = setReg regs ipR ip
        instr = V.unsafeIndex program ip
        regs'' = action instr regs'
        ip' = getReg regs'' ipR + 1
    in if ip' >= V.length program
       then getReg regs'' 0
       else go ip' regs''

import           Control.Monad.ST    (runST)
import qualified Data.Vector.Unboxed as V

import           Common
import           Utils

main :: IO ()
main = mainFor 21 parse (show . solve)

solve :: (Int, Program) -> Int
solve (ipR, program) = runST $ do
    regs <- newRegs 0 0 0 0 0 0
    go regs 6
  where
    go regs 28 = getReg regs 5
    go regs ip = do
      let instr = V.unsafeIndex program ip
      setReg regs ipR ip
      action instr regs
      ip' <- (+1) <$> getReg regs ipR
      go regs ip'
{-
Register 0 is only used once at the end, in a conditional jump:

          #ip 1
     0    seti 123 0 5
     1    bani 5 456 5
     2    eqri 5 72 5
     3    addr 5 1 1
     4    seti 0 0 1
     5    seti 0 9 5
     6    bori 5 65536 2
     7    seti 7571367 9 5
     8    bani 2 255 4
     9    addr 5 4 5
    10    bani 5 16777215 5
    11    muli 5 65899 5
    12    bani 5 16777215 5
    13    gtir 256 2 4
    14    addr 4 1 1
    15    addi 1 1 1
    16    seti 27 1 1
    17    seti 0 2 4
    18    addi 4 1 3
    19    muli 3 256 3
    20    gtrr 3 2 3
    21    addr 3 1 1
    22    addi 1 1 1
    23    seti 25 6 1
    24    addi 4 1 4
    25    seti 17 8 1
    26    setr 4 6 2
    27    seti 7 4 1
    28    eqrr 5 0 4
    29    addr 4 1 1
    30    seti 5 5 1

So the problem comes down to finding the value of register 0 which
minimises the number of times this jump is hit.

Decompiling, and making control flow explicit:

     0    r5 = 123
          r1 = 1
     1    r5 = r5 & 456
          r1 = 2
     2    r5 = (r5 == 72) ? 1 : 0
          r1 = 3
     3    r1 = r5 + 4
          goto r1                                 ; r1 = 4 or 5
     4    r1 = 1
          goto r1                                 ; r1 = 1
     5    r5 = 0
          r1 = 6
     6    r2 = r5 | 65536
          r1 = 7
     7    r5 = 7571367
          r1 = 8
     8    r4 = r2 | 255
          r1 = 9
     9    r5 = r5 + r4
          r1 = 10
    10    r5 = r5 & 16777215
          r1 = 11
    11    r5 = r5 * 65899
          r1 = 12
    12    r5 = r5 & 16777215
          r1 = 13
    13    r4 = (256 > r2) ? 1 : 0
          r1 = 14
    14    r1 = r4 + 15
          goto r1                                 ; r1 = 15 or 16
    15    r1 = 17
          goto r1                                 ; r1 = 17
    16    r1 = 28
          goto r1                                 ; r1 = 28
    17    r4 = 0
          r1 = 18
    18    r3 = r4 + 1
          r1 = 19
    19    r4 = r3 * 256
          r1 = 20
    20    r3 = (r3 > r2) ? 1 : 0
          r1 = 21
    21    r1 = r3 + 22
          goto r1                                 ; r1 = 22 or 23
    22    r1 = 24
          goto r1                                 ; r1 = 24
    23    r1 = 26
          goto r1                                 ; r1 = 26
    24    r4 = r4 + 1
          r1 = 25
    25    r1 = 18
          goto r1                                 ; r1 = 18
    26    r2 = r4
          r1 = 27
    27    r1 = 8
          goto r1                                 ; r1 = 8
    28    r4 = (r5 == r0) ? 1 : 0
          r1 = 29
    29    r1 = r4 + 30
          goto r1                                 ; r1 = 30 or 31
    30    r1 = 6
          goto r1                                 ; r1 = 6

Turning jumps into control flow structures:

    r5 = 123
    do {
      r5 = r5 & 456
      r5 = (r5 == 72) ? 1 : 0
    } while (r5 /= 72)

    r5 = 0

    do {
      r2 = r5 | 65536
      r5 = 7571367
      do {
        r4 = r2 | 255
        r5 = r5 + r4
        r5 = r5 & 16777215
        r5 = r5 * 65899
        r5 = r5 & 16777215
        r4 = (256 > r2) ? 1 : 0
        if (r4 == 1) {
          break
        }
        r4 = 0
        do {
          r3 = r4 + 1
          r4 = r3 * 256
          r3 = (r3 > r2) ? 1 : 0
          if (r3 == 1) {
            break
          }
          r4 = r4 + 1
        } while(true)
        r2 = r4
      } while (true)
      r4 = (r5 > r1) ? 1 : 0
    } while (r4 == 0)

The initial loop is pointless and ends with r5 = 72, which then
immediately gets cleared again.  So that can be removed:

    do {
      r2 = r5 | 65536
      r5 = 7571367
      do {
        r4 = r2 | 255
        r5 = r5 + r4
        r5 = r5 & 16777215
        r5 = r5 * 65899
        r5 = r5 & 16777215
        r4 = (256 > r2) ? 1 : 0
        if (r4 == 1) {
          break
        }
        r4 = 0
        do {
          r3 = r4 + 1
          r4 = r3 * 256
          r3 = (r3 > r2) ? 1 : 0
          if (r3 == 1) {
            break
          }
          r4 = r4 + 1
        } while(true)
        r2 = r4
      } while (true)
      r4 = (r5 == r0) ? 1 : 0
    } while (r4 == 0)

Inlining conditionals and permuting control flow a little:

    do {
      r2 = r5 | 65536
      r5 = 7571367
      do {
        r4 = r2 | 255
        r5 = r5 + r4
        r5 = r5 & 16777215
        r5 = r5 * 65899
        r5 = r5 & 16777215
        if (256 > r2) {
          break
        }
        r4 = 0
        do {
          r3 = r4 + 1
          r4 = r3 * 256
        } while(r3 <= r2)
        r4 = r4 - 1
        r2 = r4
      } while (true)
    } while (r5 /= r0)

I think I could work through this by hand... but that seems tedious.
So instead, the solution will start at instruction 6, and print out
the registers at instruction 28.
-}

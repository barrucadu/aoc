{-# LANGUAGE BangPatterns #-}
import Control.Monad (when)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as V

import Common
import Utils

main :: IO ()
main = mainFor 19 parse (show . solve)

solve :: (Int, Program) -> Int
solve (ipR, program) = runST $ go 0 =<< newRegs 1 0 0 0 0 0 where
  go 2 regs = do
    opt2 regs
    go 16 regs
  go ip regs = do
    let instr = V.unsafeIndex program ip
    setReg regs ipR ip
    action instr regs
    ip' <- (+1) <$> getReg regs ipR
    if ip' >= V.length program
      then getReg regs 0
      else go ip' regs

  opt2 regs = do
    r1_0 <- getReg regs 1
    r2 <- getReg regs 2
    times (r2 - r1_0 + 1) $ do
      setReg regs 3 1
      r1 <- getReg regs 1
      when (r1 `divides` r2) $ do
        r0 <- getReg regs 0
        setReg regs 0 (r0 + r1)
      r3 <- getReg regs 3
      setReg regs 3 (r3 + r1 + 1)
      setReg regs 1 (r1 + 1)
    setReg regs 1 (r2 + 1)
    setReg regs 5 0

  times n0 m = times' n0 where
    times' 0 = pure ()
    times' n = m >> times' (n-1)

  divides a b = b `mod` a == 0


{- Optimisation attempt 1

Printing (ip, instr) as the program runs shows a lot of time doing:

    (3,(2,1,3,5))
    (4,(15,5,2,5))
    (5,(0,5,4,4))
    (6,(1,4,1,4))
    (8,(1,3,1,3))
    (9,(12,3,2,5))
    (10,(0,4,5,4))
    (11,(9,2,4,4))
    (3,(2,1,3,5))
    (4,(15,5,2,5))
    (5,(0,5,4,4))
    (6,(1,4,1,4))
    (8,(1,3,1,3))
    (9,(12,3,2,5))
    (10,(0,4,5,4))
    (11,(9,2,4,4))
    (3,(2,1,3,5))
    (4,(15,5,2,5))
    ...

Decoded, that is:

     3   mulr 1 3 5    ; r5 = r1 * r3
     4   eqrr 5 2 5    ; r5 = (r5 == r2) ? 1 : 0
     5   addr 5 4 4    ; r4 += r5
     6   addi 4 1 4    ; r4 += 1
     7   addr 1 0 0    ; r0 += r1
     8   addi 3 1 3    ; r3 += 1
     9   gtrr 3 2 5    ; r5 = (r3 == r2) ? 1 : 0
    10   addr 4 5 4    ; r4 += r5
    11   seti 2 4 4    ; r4 = 2

Making the implicit ip updates explicit, we get:

     3   mulr 1 3 5    ; r5 = r1 * r3
                       ; r4 = 4
     4   eqrr 5 2 5    ; r5 = (r5 == r2) ? 1 : 0
                       ; r4 = 5
     5   addr 5 4 4    ; r4 += r5
                       ; r4 += 1
     6   addi 4 1 4    ; r4 += 1
                       ; r4 += 1
     7   addr 1 0 0    ; r0 += r1
                       ; r4 = 8
     8   addi 3 1 3    ; r3 += 1
                       ; r4 = 9
     9   gtrr 3 2 5    ; r5 = (r3 == r2) ? 1 : 0
                       ; r4 = 10
    10   addr 4 5 4    ; r4 += r5
                       ; r4 += 1
    11   seti 2 4 4    ; r4 = 2
                       ; r4 = 3

Compacting r4 additions, and propagating known values:

     3   mulr 1 3 5    ; r5 = r1 * r3
                       ; r4 = 4
     4   eqrr 5 2 5    ; r5 = (r5 == r2) ? 1 : 0
                       ; r4 = 5
     5   addr 5 4 4    ; r4 += r5 + 1
     6   addi 4 1 4    ; r4 = 8
     7   addr 1 0 0    ; r0 += r1
                       ; r4 = 8
     8   addi 3 1 3    ; r3 += 1
                       ; r4 = 9
     9   gtrr 3 2 5    ; r5 = (r3 == r2) ? 1 : 0
                       ; r4 = 10
    10   addr 4 5 4    ; r4 += r5 + 1
    11   seti 2 4 4    ; r4 = 3

This looks like a do/while with an if in the middle to me!  We can
expand the computed jumps a little:

     3   mulr 1 3 5    ; r5 = r1 * r3
                       ; r4 = 4
     4   eqrr 5 2 5    ; r5 = (r5 == r2) ? 1 : 0
                       ; r4 = 5
     5   addr 5 4 4    ; r4 = (r1 * r3 == r2) ? 7 : 6
     6   addi 4 1 4    ; r4 = 8
     7   addr 1 0 0    ; r0 += r1
                       ; r4 = 8
     8   addi 3 1 3    ; r3 += 1
                       ; r4 = 9
     9   gtrr 3 2 5    ; r5 = (r3 == r2) ? 1 : 0
                       ; r4 = 10
    10   addr 4 5 4    ; r4 = (r3 == r2) ? 12 : 11
    11   seti 2 4 4    ; r4 = 3

Stop sticking closely to the asm, and model the jumps as ifs and
loops:

do {
  r5 = r1 * r3
  r4 = 4
  r5 = (r5 == r2) ? 1 : 0
  r4 = 5
  r4 = (r1 * r3 == r2) ? 7 : 6
  if (r4 == 6) {
    r4 = 8
  }
  if (r4 == 7) {
    r0 += r1
    r4 = 8
  }
  r3 += 1
  r4 = 9
  r5 = (r3 == r2) ? 1 : 0
  r4 = 10
  r4 = (r3 == r2) ? 12 : 11
  if (r4 == 11) {
    r4 = 3
  }
} while (r4 == 3)
r4 = 12

Remove killed writes:

do {
  r4 = (r1 * r3 == r2) ? 7 : 6
  if (r4 == 7) {
    r0 += r1
  }
  r3 += 1
  r5 = (r3 == r2) ? 1 : 0
  r4 = (r3 == r2) ? 12 : 11
  if (r4 == 11) {
    r4 = 3
  }
} while (r4 == 3)
r4 = 12

Propagate conditions into control flow:

do {
  if (r1 * r3 == r2) {
    r0 += r1
  }
  r3 += 1
  r5 = (r3 == r2) ? 1 : 0
} while (r3 /= r2)
r4 = 12

Push known r5 value out of loop:

do {
  if (r1 * r3 == r2) {
    r0 += r1
  }
  r3 += 1
} while (r3 /= r2)
r5 = 1
r4 = 12

-}

{- Optimisation attempt 2

The program was still taking a long time, even with the above
optimisation.  Printing the steps out, we get:

    (2,(9,1,7,3))
    (12,(1,1,1,1))
    (13,(12,1,2,5))
    (14,(0,5,4,4))
    (15,(9,1,5,4))
    (2,(9,1,7,3))
    (12,(1,1,1,1))
    (13,(12,1,2,5))
    (14,(0,5,4,4))
    (15,(9,1,5,4))
    (2,(9,1,7,3))
    ...

Another loop starting on instruction 2!  The instructions are:

     2   seti 1 7 3
     3   above_optimisation
    12   addi 1 1 1
    13   gtrr 1 2 5
    14   addr 5 4 4
    15   seti 1 5 4

Which optimises to:

    do {
      r3 = 1
      above_optimisation
      r1 += 1
    } while (r1 <= r2)
    r5 = 0
    r4 = 16

Because `above_optimisation` doesn't write to r1 or r2, we know up
front that the loop body will run (r2 - r1 + 1) times and will end
with r1 = r2 + 1:

    do (r2 - r1 + 1) times {
      r3 = 1
      above_optimisation
      r1 += 1
    } while (r1 <= r2)
    r1 = r2 + 1
    r5 = 0
    r4 = 16

Inline `above_optimisation`:

    do (r2 - r1 + 1) times {
      r3 = 1
      do {
        if (r1 * r3 == r2) {
          r0 += r1
        }
        r3 += 1
      } while (r3 /= r2)
      r5 = 1
      r4 = 12
      r1 += 1
    } while (r1 <= r2)
    r1 = r2 + 1
    r5 = 0
    r4 = 16

And remove the redundant r4/ r5 writes:

    do (r2 - r1 + 1) times {
      r3 = 1
      do {
        if (r1 * r3 == r2) {
          r0 += r1
        }
        r3 += 1
      } while (r3 /= r2)
      r1 += 1
    } while (r1 <= r2)
    r1 = r2 + 1
    r5 = 0
    r4 = 16

Calculate how many times the inner loop will run:

    do (r2 - r1 + 1) times {
      r3 = 1
      do r1 times {
        if (r1 * r3 == r2) {
          r0 += r1
        }
        r3 += r1 + 1
      }
      r1 += 1
    } while (r1 <= r2)
    r1 = r2 + 1
    r5 = 0
    r4 = 16

That's adding k * r1 to r0, where k is the number of numbers r3 such
that 1 <= r3 <= r1 and r1 * r3 == r2.  There can only be one such
number, so this is actually checking if r1 is a factor of r2!

    do (r2 - r1 + 1) times {
      r3 = 1
      if r1 `divides` r2 {
        r0 += r1
      }
      r3 += r1 + 1
      r1 += 1
    } while (r1 <= r2)
    r1 = r2 + 1
    r5 = 0
    r4 = 16

-}

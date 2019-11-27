{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import           Control.Monad               (when)
import           Control.Monad.ST            (ST)
import           Data.Bits                   ((.&.), (.|.))
import           Data.Foldable               (for_)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

import           Utils

type Op = Int
pattern AddR <- 0  where AddR = 0
pattern AddI <- 1  where AddI = 1
pattern MulR <- 2  where MulR = 2
pattern MulI <- 3  where MulI = 3
pattern BanR <- 4  where BanR = 4
pattern BanI <- 5  where BanI = 5
pattern BorR <- 6  where BorR = 6
pattern BorI <- 7  where BorI = 7
pattern SetR <- 8  where SetR = 8
pattern SetI <- 9  where SetI = 9
pattern GtIR <- 10 where GtIR = 10
pattern GtRI <- 11 where GtRI = 11
pattern GtRR <- 12 where GtRR = 12
pattern EqIR <- 13 where EqIR = 13
pattern EqRI <- 14 where EqRI = 14
pattern EqRR <- 15 where EqRR = 15

type Program = V.Vector Instr
type Instr = (Op, Int, Int, Int)
type Regs s = VM.STVector s Int

parse :: String -> (Int, Program)
{-# INLINABLE parse #-}
parse ('#':'i':'p':' ':d:'\n':input0) = (parseDigit d, V.fromList (map goInstr (lines input0))) where
  goInstr rest =
    let (op, rest') = goOp rest
        (a, rest'') = goI rest'
        (b, rest''') = goI rest''
        (c, _) = goI rest'''
    in (op, a, b, c)

  goOp ('a':'d':'d':'r':' ':rest) = (AddR, rest)
  goOp ('a':'d':'d':'i':' ':rest) = (AddI, rest)
  goOp ('m':'u':'l':'r':' ':rest) = (MulR, rest)
  goOp ('m':'u':'l':'i':' ':rest) = (MulI, rest)
  goOp ('b':'a':'n':'r':' ':rest) = (BanR, rest)
  goOp ('b':'a':'n':'i':' ':rest) = (BanI, rest)
  goOp ('b':'o':'r':'r':' ':rest) = (BorR, rest)
  goOp ('b':'o':'r':'i':' ':rest) = (BorI, rest)
  goOp ('s':'e':'t':'r':' ':rest) = (SetR, rest)
  goOp ('s':'e':'t':'i':' ':rest) = (SetI, rest)
  goOp ('g':'t':'i':'r':' ':rest) = (GtIR, rest)
  goOp ('g':'t':'r':'i':' ':rest) = (GtRI, rest)
  goOp ('g':'t':'r':'r':' ':rest) = (GtRR, rest)
  goOp ('e':'q':'i':'r':' ':rest) = (EqIR, rest)
  goOp ('e':'q':'r':'i':' ':rest) = (EqRI, rest)
  goOp ('e':'q':'r':'r':' ':rest) = (EqRR, rest)
  goOp _ = error "invalid opcode"

  goI = goI' 0 where
    goI' !acc [] = (acc, [])
    goI' !acc (',':cs) = (acc, cs)
    goI' !acc (' ':cs) = (acc, cs)
    goI' !acc (']':cs) = (acc, cs)
    goI' !acc (c:cs) = goI' (stepParseInt acc c) cs
parse _ = error "invalid input"

action :: Instr -> Regs s -> ST s ()
{-# INLINABLE action #-}
action (AddR, a, b, c) regs = setReg regs c =<< ((+)    <$> getReg regs a <*> getReg regs b)
action (AddI, a, b, c) regs = setReg regs c =<< ((+b)   <$> getReg regs a)
action (MulR, a, b, c) regs = setReg regs c =<< ((*)    <$> getReg regs a <*> getReg regs b)
action (MulI, a, b, c) regs = setReg regs c =<< ((*b)   <$> getReg regs a)
action (BanR, a, b, c) regs = setReg regs c =<< ((.&.)  <$> getReg regs a <*> getReg regs b)
action (BanI, a, b, c) regs = setReg regs c =<< ((.&.b) <$> getReg regs a)
action (BorR, a, b, c) regs = setReg regs c =<< ((.|.)  <$> getReg regs a <*> getReg regs b)
action (BorI, a, b, c) regs = setReg regs c =<< ((.|.b) <$> getReg regs a)
action (SetR, a, _, c) regs = setReg regs c =<< getReg regs a
action (SetI, a, _, c) regs = setReg regs c a
action (GtIR, a, b, c) regs = setReg regs c . cond =<< ((a>)  <$> getReg regs b)
action (GtRI, a, b, c) regs = setReg regs c . cond =<< ((>b)  <$> getReg regs a)
action (GtRR, a, b, c) regs = setReg regs c . cond =<< ((>)   <$> getReg regs a <*> getReg regs b)
action (EqIR, a, b, c) regs = setReg regs c . cond =<< ((a==) <$> getReg regs b)
action (EqRI, a, b, c) regs = setReg regs c . cond =<< ((==b) <$> getReg regs a)
action (EqRR, a, b, c) regs = setReg regs c . cond =<< ((==)  <$> getReg regs a <*> getReg regs b)

newRegs :: Int -> Int -> Int -> Int -> Int -> Int -> ST s (Regs s)
{-# INLINABLE newRegs #-}
newRegs a b c d e f = do
  regs <- VM.new 6
  setReg regs 0 a
  setReg regs 1 b
  setReg regs 2 c
  setReg regs 3 d
  setReg regs 4 e
  setReg regs 5 f
  pure regs

getReg :: Regs s -> Int -> ST s Int
{-# INLINE getReg #-}
getReg = VM.unsafeRead

setReg :: Regs s -> Int -> Int -> ST s ()
{-# INLINE setReg #-}
setReg = VM.unsafeWrite

cond :: Bool -> Int
{-# INLINABLE cond #-}
cond True = 1
cond False = 0

-------------------------------------------------------------------------------
-- VM, see below for the heroic tale of disassembly

runProgram :: Program -> Int -> Regs s -> ST s ()
{-# INLINE runProgram #-}
runProgram program ipR regs = go 0 where
  go 2 = do
    opt2
    go 16
  go ip = do
    let instr = V.unsafeIndex program ip
    setReg regs ipR ip
    action instr regs
    ip' <- (+1) <$> getReg regs ipR
    if ip' >= V.length program
      then pure ()
      else go ip'

  opt2 = do
    r1 <- getReg regs 1
    r2 <- getReg regs 2
    for_ [r1..r2] $ \x ->
      when (x `divides` r2) $ do
        r0 <- getReg regs 0
        setReg regs 0 (r0 + x)
    setReg regs 1 (r2 + 1)
    setReg regs 3 2
    setReg regs 5 0

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
      do r2 times {
        if (r1 * r3 == r2) {
          r0 += r1
        }
        r3 += 1
      }
      r1 += 1
    } while (r1 <= r2)
    r1 = r2 + 1
    r5 = 0
    r4 = 16

That's adding k * r1 to r0, where k is the number of numbers r3 such
that 1 <= r3 <= r2 and r1 * r3 == r2.  There can only be one such
number, so this is actually checking if r1 is a factor of r2!

    do (r2 - r1 + 1) times {
      r3 = 1
      if r1 `divides` r2 {
        r0 += r1
      }
      r3 += 1
      r1 += 1
    } while (r1 <= r2)
    r1 = r2 + 1
    r5 = 0
    r4 = 16

This is summing the divisors of r2 from r1 to r2, inclusive.  As an
even better optimisation, we can implement this directly:

    for x in [r1..r2] {
      if x `divides` r2 {
        r0 += x
      }
    }
    r1 = r2 + 1
    r3 = 2
    r5 = 0
    r4 = 16

-}

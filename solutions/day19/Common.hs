{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import qualified Data.Vector.Unboxed as V
import Data.Bits ((.&.), (.|.))

import Utils

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
type Regs = (Int, Int, Int, Int, Int, Int)

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

action :: Instr -> Regs -> Regs
{-# INLINABLE action #-}
action (AddR, a, b, c) regs = setReg regs c (getReg regs a + getReg regs b)
action (AddI, a, b, c) regs = setReg regs c (getReg regs a + b)
action (MulR, a, b, c) regs = setReg regs c (getReg regs a * getReg regs b)
action (MulI, a, b, c) regs = setReg regs c (getReg regs a * b)
action (BanR, a, b, c) regs = setReg regs c (getReg regs a .&. getReg regs b)
action (BanI, a, b, c) regs = setReg regs c (getReg regs a .&. b)
action (BorR, a, b, c) regs = setReg regs c (getReg regs a .|. getReg regs b)
action (BorI, a, b, c) regs = setReg regs c (getReg regs a .|. b)
action (SetR, a, _, c) regs = setReg regs c (getReg regs a)
action (SetI, a, _, c) regs = setReg regs c a
action (GtIR, a, b, c) regs = setReg regs c (cond (a > getReg regs b))
action (GtRI, a, b, c) regs = setReg regs c (cond (getReg regs a > b))
action (GtRR, a, b, c) regs = setReg regs c (cond (getReg regs a > getReg regs b))
action (EqIR, a, b, c) regs = setReg regs c (cond (a == getReg regs b))
action (EqRI, a, b, c) regs = setReg regs c (cond (getReg regs a == b))
action (EqRR, a, b, c) regs = setReg regs c (cond (getReg regs a == getReg regs b))

getReg :: Regs -> Int -> Int
{-# INLINABLE getReg #-}
getReg (a, _, _, _, _, _) 0 = a
getReg (_, b, _, _, _, _) 1 = b
getReg (_, _, c, _, _, _) 2 = c
getReg (_, _, _, d, _, _) 3 = d
getReg (_, _, _, _, e, _) 4 = e
getReg (_, _, _, _, _, f) 5 = f
getReg _ _ = error "getReg: invalid register"

setReg :: Regs -> Int -> Int -> Regs
{-# INLINABLE setReg #-}
setReg (_, b, c, d, e, f) 0 !a = (a, b, c, d, e, f)
setReg (a, _, c, d, e, f) 1 !b = (a, b, c, d, e, f)
setReg (a, b, _, d, e, f) 2 !c = (a, b, c, d, e, f)
setReg (a, b, c, _, e, f) 3 !d = (a, b, c, d, e, f)
setReg (a, b, c, d, _, f) 4 !e = (a, b, c, d, e, f)
setReg (a, b, c, d, e, _) 5 !f = (a, b, c, d, e, f)
setReg _ _ _ = error "setReg: invalid register"

cond :: Bool -> Int
{-# INLINABLE cond #-}
cond True = 1
cond False = 0

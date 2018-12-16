{-# LANGUAGE BangPatterns #-}

module Common where

import Control.Arrow (first)
import Data.Bits ((.&.), (.|.))

import Utils

type Example = (Regs, EncodedInstr, Regs)
type Regs = (Int, Int, Int, Int)
type EncodedInstr = (Int, Int, Int, Int)

data Op
  = AddR | AddI
  | MulR | MulI
  | BanR | BanI
  | BorR | BorI
  | SetR | SetI
  | GtIR | GtRI | GtRR
  | EqIR | EqRI | EqRR
  deriving (Eq, Ord, Show, Enum, Bounded)

parse :: String -> ([Example], [EncodedInstr])
{-# INLINABLE parse #-}
parse = go . lines where
  go (('B':'e':'f':'o':'r':'e':':':' ':'[':before):instr:('A':'f':'t':'e':'r':':':' ':' ':'[':after):rest) =
    first ((goRegs before, goInstr instr, goRegs after) :) (go rest)
  go ([]:rest) = go rest
  go rest = ([], map goInstr rest)

  goRegs rest =
    let (r0, ' ':rest') = goI rest
        (r1, ' ':rest'') = goI rest'
        (r2, ' ':rest''') = goI rest''
        (r3, _) = goI rest'''
    in (r0, r1, r2, r3)

  goInstr rest =
    let (op, rest') = goI rest
        (a, rest'') = goI rest'
        (b, rest''') = goI rest''
        (c, _) = goI rest'''
    in (op, a, b, c)

  goI = goI' 0 where
    goI' !acc [] = (acc, [])
    goI' !acc (',':cs) = (acc, cs)
    goI' !acc (' ':cs) = (acc, cs)
    goI' !acc (']':cs) = (acc, cs)
    goI' !acc (c:cs) = goI' (stepParseInt acc c) cs

compatible :: Example -> Op -> Bool
{-# INLINABLE compatible #-}
compatible (regs, instr, regs') op = regs' == action op instr regs

action :: Op -> EncodedInstr -> Regs -> Regs
{-# INLINABLE action #-}
action AddR (_, a, b, c) regs = setReg regs c (getReg regs a + getReg regs b)
action AddI (_, a, b, c) regs = setReg regs c (getReg regs a + b)
action MulR (_, a, b, c) regs = setReg regs c (getReg regs a * getReg regs b)
action MulI (_, a, b, c) regs = setReg regs c (getReg regs a * b)
action BanR (_, a, b, c) regs = setReg regs c (getReg regs a .&. getReg regs b)
action BanI (_, a, b, c) regs = setReg regs c (getReg regs a .&. b)
action BorR (_, a, b, c) regs = setReg regs c (getReg regs a .|. getReg regs b)
action BorI (_, a, b, c) regs = setReg regs c (getReg regs a .|. b)
action SetR (_, a, _, c) regs = setReg regs c (getReg regs a)
action SetI (_, a, _, c) regs = setReg regs c a
action GtIR (_, a, b, c) regs = setReg regs c (cond (a > getReg regs b))
action GtRI (_, a, b, c) regs = setReg regs c (cond (getReg regs a > b))
action GtRR (_, a, b, c) regs = setReg regs c (cond (getReg regs a > getReg regs b))
action EqIR (_, a, b, c) regs = setReg regs c (cond (a == getReg regs b))
action EqRI (_, a, b, c) regs = setReg regs c (cond (getReg regs a == b))
action EqRR (_, a, b, c) regs = setReg regs c (cond (getReg regs a == getReg regs b))

getReg :: Regs -> Int -> Int
{-# INLINABLE getReg #-}
getReg (a, _, _, _) 0 = a
getReg (_, b, _, _) 1 = b
getReg (_, _, c, _) 2 = c
getReg (_, _, _, d) 3 = d
getReg _ _ = error "getReg: invalid register"

setReg :: Regs -> Int -> Int -> Regs
{-# INLINABLE setReg #-}
setReg (_, b, c, d) 0 a = (a, b, c, d)
setReg (a, _, c, d) 1 b = (a, b, c, d)
setReg (a, b, _, d) 2 c = (a, b, c, d)
setReg (a, b, c, _) 3 d = (a, b, c, d)
setReg _ _ _ = error "setReg: invalid register"

cond :: Bool -> Int
{-# INLINABLE cond #-}
cond True = 1
cond False = 0

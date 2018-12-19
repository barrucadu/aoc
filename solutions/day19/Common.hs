{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
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

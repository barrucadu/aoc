{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import           Control.Monad.ST            (ST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Utils

pattern OpAdd <- 1  where OpAdd = 1
pattern OpMul <- 2  where OpMul = 2
pattern OpHlt <- 99 where OpHlt = 99

data Instr
  = IAdd {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | IMul {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | IHlt

type Program = [Instr]
type Memory s = VUM.STVector s Int

parse :: String -> Program
parse = goOp where
  goOp ('1':',':rest) = goInA IAdd 0 rest
  goOp ('2':',':rest) = goInA IMul 0 rest
  goOp ('9':'9':',':rest) = IHlt : goOp rest
  goOp ('9':'9':[]) = [IHlt]
  goOp [] = []
  goOp xs = error ("unexpected opcode: " ++ show xs)

  goInA instr !acc (',':rest) = goInB (instr acc) 0 rest
  goInA instr !acc (c:rest) = goInA instr (stepParseInt acc c) rest
  goInA _ _ _ = error "unexpected end of input"

  goInB instr !acc (',':rest) = goOut (instr acc) 0 rest
  goInB instr !acc (c:rest) = goInB instr (stepParseInt acc c) rest
  goInB _ _ _ = error "unexpected end of input"

  goOut instr !acc (',':rest) = (instr acc) : goOp rest
  goOut instr !acc (c:rest) = goOut instr (stepParseInt acc c) rest
  goOut instr !acc [] = [instr acc]

initialise :: Program -> ST s (Memory s)
initialise program = do
    mem <- VUM.new memorySize
    go mem
    pure mem
  where
    go :: Memory s -> ST s ()
    go mem = go' 0 program where
      go' !p ((IAdd inA inB out):rest) = do
        VUM.unsafeWrite mem p OpAdd
        VUM.unsafeWrite mem (p+1) inA
        VUM.unsafeWrite mem (p+2) inB
        VUM.unsafeWrite mem (p+3) out
        go' (p+4) rest
      go' !p ((IMul inA inB out):rest) = do
        VUM.unsafeWrite mem p OpMul
        VUM.unsafeWrite mem (p+1) inA
        VUM.unsafeWrite mem (p+2) inB
        VUM.unsafeWrite mem (p+3) out
        go' (p+4) rest
      go' !p (IHlt:rest) = do
        VUM.unsafeWrite mem p OpHlt
        go' (p+1) rest
      go' _ _ = pure ()

run :: Memory s -> ST s ()
run mem = go 0 where
  go !ip = VUM.unsafeRead mem ip >>= \case
    OpAdd -> do
      inA <- VUM.unsafeRead mem (ip+1)
      inB <- VUM.unsafeRead mem (ip+2)
      out <- VUM.unsafeRead mem (ip+3)
      a <- VUM.unsafeRead mem inA
      b <- VUM.unsafeRead mem inB
      VUM.unsafeWrite mem out (a + b)
      go (ip+4)
    OpMul -> do
      inA <- VUM.unsafeRead mem (ip+1)
      inB <- VUM.unsafeRead mem (ip+2)
      out <- VUM.unsafeRead mem (ip+3)
      a <- VUM.unsafeRead mem inA
      b <- VUM.unsafeRead mem inB
      VUM.unsafeWrite mem out (a * b)
      go (ip+4)
    _ -> pure ()

-- 150 ints should be enough for anyone
memorySize :: Int
memorySize = 150

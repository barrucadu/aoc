{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Common where

import           Control.Monad.ST            (ST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Utils

pattern OpAddPP  <- 0001 where OpAddPP  = 0001
pattern OpAddPI  <- 1001 where OpAddPI  = 1001
pattern OpAddIP  <- 0101 where OpAddIP  = 0101
pattern OpAddII  <- 1101 where OpAddII  = 1101
pattern OpMulPP  <- 0002 where OpMulPP  = 0002
pattern OpMulPI  <- 1002 where OpMulPI  = 1002
pattern OpMulIP  <- 0102 where OpMulIP  = 0102
pattern OpMulII  <- 1102 where OpMulII  = 1102
pattern OpRead   <- 0003 where OpRead   = 0003
pattern OpWriteP <- 0004 where OpWriteP = 0004
pattern OpWriteI <- 0104 where OpWriteI = 0104
pattern OpJmtPP  <- 0005 where OpJmtPP  = 0005
pattern OpJmtPI  <- 1005 where OpJmtPI  = 1005
pattern OpJmtIP  <- 0105 where OpJmtIP  = 0105
pattern OpJmtII  <- 1105 where OpJmtII  = 1106
pattern OpJmfPP  <- 0006 where OpJmfPP  = 0005
pattern OpJmfPI  <- 1006 where OpJmfPI  = 1006
pattern OpJmfIP  <- 0106 where OpJmfIP  = 0106
pattern OpJmfII  <- 1106 where OpJmfII  = 1106
pattern OpLtPP   <- 0007 where OpLtPP   = 0007
pattern OpLtPI   <- 1007 where OpLtPI   = 1007
pattern OpLtIP   <- 0107 where OpLtIP   = 0107
pattern OpLtII   <- 1107 where OpLtII   = 1107
pattern OpEqPP   <- 0008 where OpEqPP   = 0008
pattern OpEqPI   <- 1008 where OpEqPI   = 1008
pattern OpEqIP   <- 0108 where OpEqIP   = 0108
pattern OpEqII   <- 1108 where OpEqII   = 1108
pattern OpHlt    <- 0099 where OpHlt    = 0099

type Program = [Int]
type Memory s = VUM.STVector s Int

parse :: String -> Program
parse = go id 0 where
  go f !acc [] = [f acc]
  go f !acc ['\n'] = [f acc]
  go f !acc (',':rest) = f acc : go id 0 rest
  go _ !acc ('-':rest) = go negate acc rest
  go f !acc (c:rest) = go f (stepParseInt acc c) rest

initialise :: Program -> ST s (Memory s)
initialise program = do
    mem <- VUM.new memorySize
    go mem
    pure mem
  where
    go :: Memory s -> ST s ()
    go mem = go' 0 program where
      go' !p (v:vs) = VUM.write mem p v >> go' (p+1) vs
      go' _ [] = pure ()

run :: Memory s -> [Int] -> ST s [Int]
run mem = go 0 where
  go !ip input = VUM.unsafeRead mem ip >>= \case
    OpAddPP -> do
      a <- readP (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a + b)
      go (ip+4) input
    OpAddPI -> do
      a <- readP (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a + b)
      go (ip+4) input
    OpAddIP -> do
      a <- readI (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a + b)
      go (ip+4) input
    OpAddII -> do
      a <- readI (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a + b)
      go (ip+4) input
    OpMulPP -> do
      a <- readP (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a * b)
      go (ip+4) input
    OpMulPI -> do
      a <- readP (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a * b)
      go (ip+4) input
    OpMulIP -> do
      a <- readI (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a * b)
      go (ip+4) input
    OpMulII -> do
      a <- readI (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (a * b)
      go (ip+4) input
    OpRead -> do
      out <- readI (ip+1)
      VUM.unsafeWrite mem out (head input)
      go (ip+2) (tail input)
    OpWriteP -> do
      a <- readP (ip+1)
      (a:) <$> go (ip+2) input
    OpWriteI -> do
      a <- readI (ip+1)
      (a:) <$> go (ip+2) input
    OpJmtPP -> do
      a <- readP (ip+1)
      b <- readP (ip+2)
      go (if a /= 0 then b else ip+3) input
    OpJmtPI -> do
      a <- readP (ip+1)
      b <- readI (ip+2)
      go (if a /= 0 then b else ip+3) input
    OpJmtIP -> do
      a <- readI (ip+1)
      b <- readP (ip+2)
      go (if a /= 0 then b else ip+3) input
    OpJmtII -> do
      a <- readI (ip+1)
      b <- readI (ip+2)
      go (if a /= 0 then b else ip+3) input
    OpJmfPP -> do
      a <- readP (ip+1)
      b <- readP (ip+2)
      go (if a == 0 then b else ip+3) input
    OpJmfPI -> do
      a <- readP (ip+1)
      b <- readI (ip+2)
      go (if a == 0 then b else ip+3) input
    OpJmfIP -> do
      a <- readI (ip+1)
      b <- readP (ip+2)
      go (if a == 0 then b else ip+3) input
    OpJmfII -> do
      a <- readI (ip+1)
      b <- readI (ip+2)
      go (if a == 0 then b else ip+3) input
    OpLtPP -> do
      a <- readP (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a < b then 1 else 0)
      go (ip+4) input
    OpLtPI -> do
      a <- readP (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a < b then 1 else 0)
      go (ip+4) input
    OpLtIP -> do
      a <- readI (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a < b then 1 else 0)
      go (ip+4) input
    OpLtII -> do
      a <- readI (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a < b then 1 else 0)
      go (ip+4) input
    OpEqPP -> do
      a <- readP (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a == b then 1 else 0)
      go (ip+4) input
    OpEqPI -> do
      a <- readP (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a == b then 1 else 0)
      go (ip+4) input
    OpEqIP -> do
      a <- readI (ip+1)
      b <- readP (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a == b then 1 else 0)
      go (ip+4) input
    OpEqII -> do
      a <- readI (ip+1)
      b <- readI (ip+2)
      out <- readI (ip+3)
      VUM.unsafeWrite mem out (if a == b then 1 else 0)
      go (ip+4) input
    OpHlt -> pure []
    i -> error ("unexpected opcode: " ++ show i)
    
  readI = VUM.unsafeRead mem
  readP addr = VUM.unsafeRead mem =<< VUM.unsafeRead mem addr

-- 1024 ints should be enough for anyone
memorySize :: Int
memorySize = 1024

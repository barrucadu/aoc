{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Intcode where

import           Control.Monad               (void)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (ST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Utils

pattern OpAddPP  <- 0001
pattern OpAddPI  <- 1001
pattern OpAddIP  <- 0101
pattern OpAddII  <- 1101
pattern OpMulPP  <- 0002
pattern OpMulPI  <- 1002
pattern OpMulIP  <- 0102
pattern OpMulII  <- 1102
pattern OpRead   <- 0003
pattern OpWriteP <- 0004
pattern OpWriteI <- 0104
pattern OpJmtPP  <- 0005
pattern OpJmtPI  <- 1005
pattern OpJmtIP  <- 0105
pattern OpJmtII  <- 1105
pattern OpJmfPP  <- 0006
pattern OpJmfPI  <- 1006
pattern OpJmfIP  <- 0106
pattern OpJmfII  <- 1106
pattern OpLtPP   <- 0007
pattern OpLtPI   <- 1007
pattern OpLtIP   <- 0107
pattern OpLtII   <- 1107
pattern OpEqPP   <- 0008
pattern OpEqPI   <- 1008
pattern OpEqIP   <- 0108
pattern OpEqII   <- 1108
pattern OpHlt    <- 0099

type Program = [Int]
type Memory m = VUM.STVector (PrimState m) Int

parse :: String -> Program
{-# INLINABLE parse #-}
parse = go id 0 where
  go f !acc [] = [f acc]
  go f !acc ['\n'] = [f acc]
  go f !acc (',':rest) = f acc : go id 0 rest
  go _ !acc ('-':rest) = go negate acc rest
  go f !acc (c:rest) = go f (stepParseInt acc c) rest

initialise :: PrimMonad m => Program -> m (Memory m)
{-# INLINABLE initialise #-}
initialise program = do
    mem <- VUM.new (length program)
    go mem
    pure mem
  where
    go mem = go' 0 program where
      go' !p (v:vs) = VUM.write mem p v >> go' (p+1) vs
      go' _ [] = pure ()

runNoIO :: PrimMonad m => Memory m -> m ()
{-# INLINABLE runNoIO #-}
runNoIO mem = void (run mem [])

run :: PrimMonad m => Memory m -> [Int] -> m [Int]
{-# INLINABLE run #-}
run mem = go (runPartial mem 0) where
  go k input@(i:is) = k >>= \case
    In  k' m -> m i >> go k' is
    Out k' a -> (a:) <$> go k' input
    Stop -> pure []
  go k [] = k >>= \case
    In  _ _ -> error "expected input but there is none"
    Out k' a -> (a:) <$> go k' []
    Stop -> pure []

data Partial m
  = In  (m (Partial m)) (Int -> m ())
  | Out (m (Partial m)) {-# UNPACK #-} !Int
  | Stop

runPartial :: PrimMonad m => Memory m -> Int -> m (Partial m)
{-# INLINABLE runPartial #-}
runPartial mem = go where
  go !ip = readI ip >>= \case
      OpAddPP -> primBinOp (+) readP readP
      OpAddPI -> primBinOp (+) readP readI
      OpAddIP -> primBinOp (+) readI readP
      OpAddII -> primBinOp (+) readI readI
      OpMulPP -> primBinOp (*) readP readP
      OpMulPI -> primBinOp (*) readP readI
      OpMulIP -> primBinOp (*) readI readP
      OpMulII -> primBinOp (*) readI readI
      OpJmtPP -> primJumpIf (/=0) readP readP
      OpJmtPI -> primJumpIf (/=0) readP readI
      OpJmtIP -> primJumpIf (/=0) readI readP
      OpJmtII -> primJumpIf (/=0) readI readI
      OpJmfPP -> primJumpIf (==0) readP readP
      OpJmfPI -> primJumpIf (==0) readP readI
      OpJmfIP -> primJumpIf (==0) readI readP
      OpJmfII -> primJumpIf (==0) readI readI
      OpLtPP -> primCompare (<)  readP readP
      OpLtPI -> primCompare (<)  readP readI
      OpLtIP -> primCompare (<)  readI readP
      OpLtII -> primCompare (<)  readI readI
      OpEqPP -> primCompare (==) readP readP
      OpEqPI -> primCompare (==) readP readI
      OpEqIP -> primCompare (==) readI readP
      OpEqII -> primCompare (==) readI readI
      OpRead -> pure . In (go (ip+2)) $ \a -> do
        out <- readI (ip+1)
        VUM.unsafeWrite mem out a
      OpWriteP -> Out (go (ip+2)) <$> readP (ip+1)
      OpWriteI -> Out (go (ip+2)) <$> readI (ip+1)
      OpHlt -> pure Stop
      i -> error ("unexpected opcode: " ++ show i)
    where
      primBinOp f readA readB = do
        a <- readA (ip+1)
        b <- readB (ip+2)
        out <- readI (ip+3)
        VUM.unsafeWrite mem out (f a b)
        go (ip+4)

      primJumpIf f readA readB = do
        a <- readA (ip+1)
        b <- readB (ip+2)
        go (if f a then b else ip+3)

      primCompare f readA readB = do
        a <- readA (ip+1)
        b <- readB (ip+2)
        out <- readI (ip+3)
        VUM.unsafeWrite mem out (if f a b then 1 else 0)
        go (ip+4)

  readI = VUM.unsafeRead mem
  readP addr = VUM.unsafeRead mem =<< VUM.unsafeRead mem addr

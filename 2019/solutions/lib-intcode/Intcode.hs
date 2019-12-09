{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intcode where

import           Control.Monad               (void)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (ST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Utils

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
    mem <- VUM.new 2048
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
run mem = go (runPartial mem) where
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

pattern OpAdd   <- 1 :: Int
pattern OpMul   <- 2 :: Int
pattern OpRead  <- 3 :: Int
pattern OpWrite <- 4 :: Int
pattern OpJmT   <- 5 :: Int
pattern OpJmF   <- 6 :: Int
pattern OpLt    <- 7 :: Int
pattern OpEq    <- 8 :: Int
pattern OpChRb  <- 9 :: Int
pattern OpHalt  <- 99 :: Int

pattern ArgP <- 0 :: Int
pattern ArgI <- 1 :: Int
pattern ArgR <- 2 :: Int

runPartial :: forall m. PrimMonad m => Memory m -> m (Partial m)
{-# INLINABLE runPartial #-}
runPartial mem = go 0 0 where
  go !rb !ip = decode =<< readI ip where
    decode :: Int -> m (Partial m)
    decode instr = case instr `mod` 100 of
      OpAdd   -> primBinOp (+) (decodeReadArg 0 instr) (decodeReadArg 1 instr) (decodeWriteArg 2 instr)
      OpMul   -> primBinOp (*) (decodeReadArg 0 instr) (decodeReadArg 1 instr) (decodeWriteArg 2 instr)
      OpRead  -> primRead (decodeWriteArg 0 instr)
      OpWrite -> primWrite (decodeReadArg 0 instr)
      OpJmT   -> primJumpIf (/=0) (decodeReadArg 0 instr) (decodeReadArg 1 instr)
      OpJmF   -> primJumpIf (==0) (decodeReadArg 0 instr) (decodeReadArg 1 instr)
      OpLt    -> primCompare (<) (decodeReadArg 0 instr) (decodeReadArg 1 instr) (decodeWriteArg 2 instr)
      OpEq    -> primCompare (==) (decodeReadArg 0 instr) (decodeReadArg 1 instr) (decodeWriteArg 2 instr)
      OpChRb  -> primChRb (decodeReadArg 0 instr)
      OpHalt  -> pure Stop
      op -> error ("unexpected opcode: " ++ show op ++ " (instruction: " ++ show instr ++ ")")

    decodeReadArg :: Int -> Int -> Int -> m Int
    decodeReadArg i instr
      | instr < pos = readP
      | otherwise = case (instr `div` pos) `mod` 10 of
          ArgP -> readP
          ArgI -> readI
          ArgR -> readR rb
          mode -> error ("unexpected read parameter mode: " ++ show mode ++ " in position " ++ show i ++ " (instruction: " ++ show instr ++ ")")
      where
        pos = 10^(i+2)

    decodeWriteArg :: Int -> Int -> Int -> Int -> m ()
    decodeWriteArg i instr
      | instr < pos = writeP
      | otherwise = case (instr `div` pos) `mod` 10 of
          ArgP -> writeP
          ArgR -> writeR rb
          mode -> error ("unexpected write parameter mode: " ++ show mode ++ " in position " ++ show i ++ " (instruction: " ++ show instr ++ ")")
      where
        pos = 10^(i+2)

    primBinOp f read0 read1 write2 = do
      a <- read0 (ip+1)
      b <- read1 (ip+2)
      write2 (ip+3) (f a b)
      go rb (ip+4)

    primJumpIf f read0 read1 = do
      a <- read0 (ip+1)
      b <- read1 (ip+2)
      go rb (if f a then b else ip+3)

    primCompare f read0 read1 write2 = do
      a <- read0 (ip+1)
      b <- read1 (ip+2)
      write2 (ip+3) (if f a b then 1 else 0)
      go rb (ip+4)

    primRead write0 = pure $ In (go rb (ip+2)) (write0 (ip+1))

    primWrite read0 = Out (go rb (ip+2)) <$> read0 (ip+1)

    primChRb read0 = do
      rbOff <- read0 (ip+1)
      go (rb + rbOff) (ip+2)

  readI = VUM.unsafeRead mem
  readP addr = VUM.unsafeRead mem =<< VUM.unsafeRead mem addr
  readR rb addr = do
    off <- VUM.unsafeRead mem addr
    VUM.unsafeRead mem (rb + off)

  writeP addr a = do
    target <- VUM.unsafeRead mem addr
    VUM.unsafeWrite mem target a
  writeR rb addr a = do
    off <- VUM.unsafeRead mem addr
    VUM.unsafeWrite mem (rb + off) a

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

type Program = [Int]
type Memory s = VUM.STVector s Int

parse :: String -> Program
parse = go 0 where
  go !acc [] = [acc]
  go !acc ['\n'] = [acc]
  go !acc (',':rest) = acc : go 0 rest
  go !acc (c:rest) = go (stepParseInt acc c) rest

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

run :: Memory s -> ST s ()
run mem = go 0 where
  go !ip = VUM.unsafeRead mem ip >>= \case
    OpAdd -> do
      a <- VUM.unsafeRead mem =<< VUM.unsafeRead mem (ip+1)
      b <- VUM.unsafeRead mem =<< VUM.unsafeRead mem (ip+2)
      out <- VUM.unsafeRead mem (ip+3)
      VUM.unsafeWrite mem out (a + b)
      go (ip+4)
    OpMul -> do
      a <- VUM.unsafeRead mem =<< VUM.unsafeRead mem (ip+1)
      b <- VUM.unsafeRead mem =<< VUM.unsafeRead mem (ip+2)
      out <- VUM.unsafeRead mem (ip+3)
      VUM.unsafeWrite mem out (a * b)
      go (ip+4)
    _ -> pure ()

-- 150 ints should be enough for anyone
memorySize :: Int
memorySize = 150

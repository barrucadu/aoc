import           Control.Concurrent.Async (async, wait)
import           Control.Concurrent.MVar  (newEmptyMVar, newMVar, putMVar,
                                           takeMVar)
import           Data.List                (permutations)

import           Common
import           Utils

main :: IO ()
main = iomainFor 7 parse (fmap show . solve)

solve :: Program -> IO Int
solve program = go 0 (permutations [0,1,2,3,4]) where
  go best [] = pure best
  go best (phase:phases) = do
    candidate <- go' phase
    go (max candidate best) phases

  go' [pA,pB,pC,pD,pE] = runNetwork program pA pB pC pD pE
  go' _ = error "invalid phase specification"

runNetwork :: Program -> Int -> Int -> Int -> Int -> Int -> IO Int
runNetwork program pA pB pC pD pE = do
  memoryA <- initialise program
  memoryB <- initialise program
  memoryC <- initialise program
  memoryD <- initialise program
  memoryE <- initialise program

  inputA  <- newMVar pA
  inputB  <- newMVar pB
  inputC  <- newMVar pC
  inputD  <- newMVar pD
  inputE  <- newMVar pE
  outputE <- newEmptyMVar

  a <- async $ runOne memoryA (putMVar inputB)  (takeMVar inputA)
  b <- async $ runOne memoryB (putMVar inputC)  (takeMVar inputB)
  c <- async $ runOne memoryC (putMVar inputD)  (takeMVar inputC)
  d <- async $ runOne memoryD (putMVar inputE)  (takeMVar inputD)
  e <- async $ runOne memoryE (putMVar outputE) (takeMVar inputE)

  putMVar inputA 0

  wait a
  wait b
  wait c
  wait d
  wait e

  takeMVar outputE

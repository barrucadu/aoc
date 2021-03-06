{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Common where

import           Control.Concurrent.Async    (async, wait)
import           Control.Concurrent.MVar     (newMVar, putMVar, takeMVar)
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           System.IO.Unsafe            (unsafePerformIO)

import           Intcode

-- this is actually safe because there are no externally visible effects
runNetwork :: Program -> Int -> Int -> Int -> Int -> Int -> Int
runNetwork program pA pB pC pD pE = unsafePerformIO $ do
    mem0 <- initialise program

    (kA, inputA) <- setup mem0 pA
    (kB, inputB) <- setup mem0 pB
    (kC, inputC) <- setup mem0 pC
    (kD, inputD) <- setup mem0 pD
    (kE, inputE) <- setup mem0 pE

    out <- newIORef 0
    let doOutputE i = writeIORef out i >> putMVar inputA i

    a <- async $ runOne (putMVar inputB) (takeMVar inputA) kA
    b <- async $ runOne (putMVar inputC) (takeMVar inputB) kB
    c <- async $ runOne (putMVar inputD) (takeMVar inputC) kC
    d <- async $ runOne (putMVar inputE) (takeMVar inputD) kD
    e <- async $ runOne doOutputE        (takeMVar inputE) kE

    putMVar inputA 0

    wait a
    wait b
    wait c
    wait d
    wait e

    readIORef out
  where
    setup mem0 phase = do
      memory <- VUM.clone mem0
      input  <- newMVar phase
      pure (runPartial memory, input)

    runOne :: (Int -> IO ()) -> IO Int -> IO (Partial IO) -> IO ()
    runOne put get = go where
      go k = k >>= \case
        In k' m -> do
          m =<< get
          go k'
        Out k' a -> do
          put a
          go k'
        Stop -> pure ()

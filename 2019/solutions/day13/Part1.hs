{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Intcode
import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: Program -> Int
solve program = runST $ do
    mem <- VUM.new 4096
    initialise' mem program
    go 0 (runPartial mem)
  where
    go !blocks k = k >>= \case
      Out k' _ -> k' >>= \case
        Out k'' _ -> k'' >>= \case
          Out k''' 2 -> go (blocks+1) k'''
          Out k''' _ -> go blocks k'''
          In _ _ -> error "unexpected In instruction in Out/Out"
          Stop -> pure blocks
        In _ _ -> error "unexpected In instruction in Out"
        Stop -> pure blocks
      In _ _ -> error "unexpected In instruction"
      Stop -> pure blocks

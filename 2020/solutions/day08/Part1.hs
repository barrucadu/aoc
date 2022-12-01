{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad.ST            (runST)
import           Data.Foldable               (for_)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Common
import           Utils

main :: IO ()
main = mainFor 8 parse (show . solve)

solve :: [Instruction] -> Int
solve instructions = runST $ do
    memory <- VUM.new (length instructions)
    for_ (zip [0..] instructions) $ \(i, instr) -> VUM.write memory i (False, instr)
    solve memory
  where
    solve memory = solve' 0 0 where
      solve' !acc !i = VUM.read memory i >>= \case
        (True, _) -> pure acc
        (False, instr) -> VUM.write memory i (True, instr) >> next acc i instr

      next !acc !i (Acc, off) = solve' (acc+off) (i+1)
      next !acc !i (Jmp, off) = solve' acc (i+off)
      next !acc !i _  = solve' acc (i+1)

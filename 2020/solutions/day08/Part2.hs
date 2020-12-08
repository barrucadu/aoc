{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Foldable (for_)

import Common
import Utils

main :: IO ()
main = mainFor 8 parse (show . solve)

solve :: [Instruction] -> Int
solve instructions = runST $ do
    memory <- VUM.new size
    for_ (zip [0..] instructions) $ \(i, instr) -> VUM.write memory i (False, instr)
    solve memory      
  where
    size = length instructions
    
    solve memory0 = solve' 0 where
      solve' jmpn = do
        memory <- VUM.clone memory0
        toggleJmp memory jmpn
        checkLoop memory >>= \case
          Just acc -> pure acc
          Nothing -> solve' (jmpn+1)

      toggleJmp memory = toggleJmp' 0 where
        toggleJmp' !i !n = VUM.read memory i >>= \case
          (b, (Jmp, off))
            | n == 0 -> VUM.write memory i (b, (Nop, off))
            | otherwise -> toggleJmp' (i+1) (n-1)
          (b, (Nop, off))
            | n == 0 -> VUM.write memory i (b, (Jmp, off))
            | otherwise -> toggleJmp' (i+1) (n-1)
          _ -> toggleJmp'(i+1) n

      checkLoop memory = checkLoop' 0 0 where
        checkLoop' !acc !i | i == size = pure (Just acc)
        checkLoop' !acc !i = VUM.read memory i >>= \case
          (True, _) -> pure Nothing
          (False, instr) -> VUM.write memory i (True, instr) >> next acc i instr

        next !acc !i (Acc, off) = checkLoop' (acc+off) (i+1)
        next !acc !i (Jmp, off) = checkLoop' acc (i+off)
        next !acc !i _  = checkLoop' acc (i+1)

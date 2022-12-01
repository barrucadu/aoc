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
    memory <- VUM.new size
    for_ (zip [0..] instructions) $ \(i, instr) -> VUM.write memory i instr
    solve memory
  where
    size = length instructions

    solve memory = solve' 0 where
      solve' jmpn = do
        track <- VUM.replicate size False
        checkLoop track jmpn >>= \case
          Just acc -> pure acc
          Nothing -> solve' (jmpn+1)

      checkLoop track = checkLoop' 0 0 where
        checkLoop' !acc !i !n
          | i == size = pure (Just acc)
          | otherwise = VUM.read track i >>= \case
            True -> pure Nothing
            False -> do
              VUM.write track i True
              next acc i n =<< VUM.read memory i

        next !acc !i !n (Acc, off) = checkLoop' (acc+off) (i+1) n
        next !acc !i !n (Jmp, off)
          | n == 0    = checkLoop' acc (i+1)   (n-1) -- nop
          | otherwise = checkLoop' acc (i+off) (n-1) -- jmp
        next !acc !i !n (_, off)
          | n == 0    = checkLoop' acc (i+off) (n-1) -- jmp
          | otherwise = checkLoop' acc (i+1)   (n-1) -- nop

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad.ST (runST)
import qualified Data.Map.Strict  as M
import           Data.Maybe       (fromJust)
import           Data.Ord         (Ordering(..), compare)

import           Common
import           Utils

main :: IO ()
main = mainFor 15 id (show . solve)

solve :: String -> Int
solve input0 = runST $ do
    (arr, es, gs) <- parse input0
    go arr es gs
  where
    go arr0 es0 gs0 = search 1 1 where
      search !n' !n = evaluate n >>= \case
        Just (GT, _) -> bsearch n' n
        Just (EQ, _) -> bsearch n' n
        _ -> search n (n*2)

      bsearch !lo !hi
        | lo == hi = snd . fromJust <$> evaluate lo
        | otherwise =
          let m = (hi - lo) `div` 2 + lo
          in evaluate m >>= \case
               Just (GT, _) -> bsearch lo (m - 1)
               Just (EQ, _) -> bsearch lo (m - 1)
               _ -> bsearch (m + 1) hi

      evaluate n = do
        arr <- cloneArray arr0
        (turns, score, units, victor) <- battle n 3 arr es0 gs0
        pure $
          if victor == Elf
          then Just (compare units target, turns * score)
          else Nothing

      target = M.size es0

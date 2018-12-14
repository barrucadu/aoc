import Control.Monad.ST (runST)
import Data.Char (intToDigit)
import qualified Data.Vector.Unboxed.Mutable as V

import Utils

main :: IO ()
main = mainFor 14 (parseInt . init) (map intToDigit . solve)

solve :: Int -> [Int]
solve p = runST $ do
    v <- V.new maxlen
    V.unsafeWrite v 0 3
    V.unsafeWrite v 1 7
    run v 2 0 1
    score v
  where
    maxlen = p + 11

    run v = go where
      go len pos1 pos2
        | len >= maxlen = pure ()
        | otherwise = do
          val1 <- V.unsafeRead v pos1
          val2 <- V.unsafeRead v pos2
          len' <- write len val1 val2
          go len' (advance len' pos1 val1) (advance len' pos2 val2)

      write len val1 val2 = case (val1 + val2) `divMod` 10 of
        (0, o) -> do
          V.unsafeWrite v len o
          pure (len+1)
        (t, o) -> do
          V.unsafeWrite v len t
          V.unsafeWrite v (len+1) o
          pure (len+2)

      advance len pos val = (pos + 1 + val) `mod` len

    score v = go [] 0 where
      go acc 10 = pure (reverse acc)
      go acc n = do
        val <- V.unsafeRead v (p + n)
        go (val:acc) (n+1)

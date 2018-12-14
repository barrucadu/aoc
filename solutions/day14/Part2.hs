import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as V

import Utils

main :: IO ()
main = mainFor 14 (map parseDigit . init) (show . solve)

solve :: [Int] -> Int
solve p = runST $ do
    v <- V.new 1024
    V.write v 0 3
    V.write v 1 7
    run v 2 0 1
  where
    wanted = reverse p
    wlen = length wanted

    run :: V.STVector s Int -> Int -> Int -> Int -> ST s Int
    run = go where
      go v len pos1 pos2
        | len >= V.length v - 2 = do
            v' <- V.unsafeGrow v (V.length v)
            go v' len pos1 pos2
        | otherwise = do
            val1 <- V.read v pos1
            val2 <- V.read v pos2
            len' <- write v len val1 val2
            doneA <- check v len'
            if doneA
              then pure (len' - wlen)
              else do
                doneB <- check v (len' - 1)
                if doneB
                  then pure (len' - 1 - wlen)
                  else go v len' (advance len' pos1 val1) (advance len' pos2 val2)

      write v len val1 val2 = case (val1 + val2) `divMod` 10 of
        (0, o) -> do
          V.write v len o
          pure (len+1)
        (t, o) -> do
          V.write v len t
          V.write v (len+1) o
          pure (len+2)

      advance len pos val = (pos + 1 + val) `mod` len

    check v len
        | len < wlen = pure False
        | otherwise = go len wanted
      where
        go 0 _ = pure True
        go _ [] = pure True
        go n (w:ws) = do
          x <- V.read v (n-1)
          if x == w
            then go (n-1) ws
            else pure False

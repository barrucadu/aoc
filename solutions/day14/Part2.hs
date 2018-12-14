import Control.Monad.ST (ST, runST)
import Data.List (isPrefixOf)
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

    run :: V.STVector s Int -> Int -> Int -> Int -> ST s Int
    run = go [] where
      go written v len pos1 pos2
        | wanted `isPrefixOf` written = pure (len - length wanted)
        | wanted `isPrefixOf` drop 1 written = pure (len - length wanted - 1)
        | len >= V.length v - 2 = do
            v' <- V.unsafeGrow v (V.length v)
            go written v' len pos1 pos2
        | otherwise = do
          val1 <- V.read v pos1
          val2 <- V.read v pos2
          (written', len') <- write v written len val1 val2
          go written' v len' (advance len' pos1 val1) (advance len' pos2 val2)

      write v written len val1 val2 = case (val1 + val2) `divMod` 10 of
        (0, o) -> do
          V.write v len o
          pure (o : written, len+1)
        (t, o) -> do
          V.write v len t
          V.write v (len+1) o
          pure (o : t : written, len+2)

      advance len pos val = (pos + 1 + val) `mod` len

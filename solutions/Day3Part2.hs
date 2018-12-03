{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)
import Data.Traversable (for)
import qualified Data.Vector.Unboxed.Mutable as V

import Utils

main :: IO ()
main = do
  input <- lines <$> readFile "../inputs/day3.txt"
  putStrLn (solve input)

solve :: [String] -> String
solve claims = runST $ do
    v <- V.new size
    let claims' = map parseClaim claims
    for_ claims' (stakeClaim v . snd)
    findUnique v claims'
  where
    size = 1000 * 1000

    parseClaim ('#':rest) = go [] rest where
      go acc (' ':'@':' ':cs) = (reverse acc, go2 0 cs)
      go acc (c:cs) = go (c:acc) cs
      go _ [] = error "invalid claim"

      go2 !acc (',':cs) = acc : go2 0 cs
      go2 !acc (':':cs) = acc : go2 0 cs
      go2 !acc ('x':cs) = acc : go2 0 cs
      go2 _ (' ':cs)   = go2 0 cs
      go2 !acc (c:cs) = go2 (stepParseInt acc c) cs
      go2 !acc [] = [acc]
    parseClaim _ = error "invalid claim"

    stakeClaim v [x0, y0, width, height] =
      for_ [x0..x0 + width - 1] $ \x ->
      for_ [y0..y0 + height - 1] $ \y ->
      V.unsafeModify v (+1) (x + y * 1000)
    stakeClaim _ _ = error "invalid claim"

    findUnique :: V.STVector s Int -> [(String, [Int])] -> ST s String
    findUnique v = go where
      go ((cid, rect):cs) = do
        ok <- checkNonOverlapping rect
        if ok then pure cid else go cs
      go [] = error "no non-overlapping claim"

      checkNonOverlapping [x0, y0, width, height] = do
        xok <- for [x0..x0 + width - 1] $ \x -> do
          yok <- for [y0..y0 + height - 1] $ \y ->
            (==1) <$> V.unsafeRead v (x + y * 1000)
          pure (and yok)
        pure (and xok)
      checkNonOverlapping _ = error "invalid claim"

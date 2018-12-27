{-# LANGUAGE BangPatterns #-}

import           Control.Monad.ST            (ST, runST)
import           Data.Foldable               (for_)
import qualified Data.Vector.Unboxed.Mutable as V

import           Utils

main :: IO ()
main = mainFor 3 lines (show . solve)

solve :: [String] -> Int
solve claims = runST $ do
    v <- V.new size
    for_ claims (stakeClaim v . parseClaim)
    countOverlaps v
  where
    size = 1000 * 1000

    parseClaim ('#':rest) = go rest where
      go (' ':'@':' ':cs) = go2 0 cs
      go (_:cs) = go cs
      go [] = error "invalid claim"

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

    countOverlaps :: V.STVector s Int -> ST s Int
    countOverlaps v = go 0 0 where
      go !acc !pos
        | pos == size = pure acc
        | otherwise = do
          current <- V.unsafeRead v pos
          go (acc + if current > 1 then 1 else 0) (pos + 1)

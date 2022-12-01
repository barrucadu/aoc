import           Control.Monad.ST (ST, runST)
import           Data.List        (sort)

import           Common
import           Utils

main :: IO ()
main = mainFor 9 id (show . solve)

solve :: String -> Int
solve input = runST $ do
    heightmap <- parseM input
    minima <- findMinima heightmap
    counts <- traverse (\(_, x, y) -> flood heightmap x y) minima
    let (a:b:c:_) = reverse (sort counts)
    pure (a * b * c)
  where
    flood :: STArray s Int -> Int -> Int -> ST s Int
    flood arr = go where
      maxX = widthArray' arr
      maxY = heightArray' arr

      go x y = do
        this <- readArray arr x y
        if this == 9
          then pure 0
          else do
            writeArray arr x y 9
            countUp <- if y == 0 then pure 0 else go x (y-1)
            countDown <- if y == maxY-1 then pure 0 else go x (y+1)
            countLeft <- if x == 0 then pure 0 else go (x-1) y
            countRight <- if x == maxX-1 then pure 0 else go(x+1) y
            pure (1 + countUp + countDown + countLeft + countRight)

import Control.Monad.ST (runST)

import Common
import Utils

main :: IO ()
main = mainFor 9 id (show . solve)

solve :: String -> Int
solve input = runST $ do
  heightmap <- parseM input
  minima <- findMinima heightmap
  pure $ sum [height + 1 | (height, _, _) <- minima]

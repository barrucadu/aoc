import Control.Monad.ST (runST)

import Common
import Utils

main :: IO ()
main = mainFor 15 id (show . solve)

solve :: String -> Int
solve input0 = runST $ do
  (arr, es, gs) <- parse input0
  (turns, score, _, _) <- battle 3 3 arr es gs
  pure (turns * score)

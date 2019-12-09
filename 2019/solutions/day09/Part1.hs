import           Control.Monad.ST (runST)

import           Intcode
import           Utils

main :: IO ()
main = mainFor 9 parse (show . solve)

solve :: Program -> Int
solve program = runST $ do
  memory <- initialise program
  head <$> run memory [1]

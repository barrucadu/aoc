import           Control.Monad.ST (runST)

import           Common
import           Utils

main :: IO ()
main = mainFor 5 parse (show . solve)

solve :: Program -> Int
solve program = runST $ do
  mem <- initialise program
  last <$> run mem [5]

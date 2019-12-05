import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Common
import           Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

solve :: Program -> Int
solve program = runST $ do
  mem <- initialise program
  VUM.unsafeWrite mem 1 12
  VUM.unsafeWrite mem 2 2
  run mem
  VUM.unsafeRead mem 0

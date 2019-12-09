import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Intcode
import           Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

solve :: Program -> Int
solve program = runST $ search [(noun, verb) | noun <- [0..99], verb <- [0..99]] where
  search ((noun, verb):rest) = do
    mem <- initialise program
    VUM.unsafeWrite mem 1 noun
    VUM.unsafeWrite mem 2 verb
    runNoIO mem
    res <- VUM.unsafeRead mem 0
    if res == target
      then pure (100 * noun + verb)
      else search rest
  search [] = error "noun and verb not found"

target :: Int
target = 19690720

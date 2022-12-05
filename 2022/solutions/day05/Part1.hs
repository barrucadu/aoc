import qualified Data.IntMap as M
import           Data.List   (foldl')

import           Common
import           Utils

main :: IO ()
main = mainFor 5 parse solve

solve :: (Stacks, [Move]) -> String
solve (stacks, moves) =
  showStacks $ foldl' doMove stacks moves

doMove :: Stacks -> Move -> Stacks
doMove s (Move moveN moveFrom moveTo) =
  let (before, after) = splitAt moveN (s M.! moveFrom)
  in M.adjust (reverse before++) moveTo $ M.insert moveFrom after s

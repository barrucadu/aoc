import           Data.List (foldl')

import           Common
import           Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: ([Bool], Transitions) -> Int
solve (initial, transitions) = calc (step 20 tape0) where
  tape0 =
    let blanks = repeat False
    in take lbuffer blanks ++ initial ++ take rbuffer blanks

  lbuffer = 5
  rbuffer = 50

  calc = foldl' go 0 . zip [negate lbuffer ..] where
    go acc (n, True) = acc + n
    go acc _ = acc

  step :: Int -> [Bool] -> [Bool]
  step 0 tape = tape
  step n tape = step (n-1) (go False False tape) where
    go l2 l1 (c:tape'@(r1:r2:_)) =
      let c' = findTransition transitions c (toK l2 l1 r1 r2)
      in c' : go l1 c tape'
    go _ _ xs = xs

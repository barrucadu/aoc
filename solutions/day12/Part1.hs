import Data.List (foldl')
import qualified Data.Map.Strict as M

import Common
import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: (String, M.Map String Char) -> Int
solve (initial, transitions) = calc (step 20 tape0) where
  tape0 =
    let blanks = repeat '.'
    in take lbuffer blanks ++ initial ++ take rbuffer blanks

  lbuffer = 5
  rbuffer = 50

  calc = foldl' go 0 . zip [negate lbuffer ..] where
    go acc (n, '#') = acc + n
    go acc _ = acc

  step :: Int -> String -> String
  step 0 tape = tape
  step n tape = step (n-1) (go '.' '.' tape) where
    go l2 l1 (c:tape'@(r1:r2:_)) =
      let c' = M.findWithDefault '.' [l2, l1, c, r1, r2] transitions
      in c' : go l1 c tape'
    go _ _ xs = xs

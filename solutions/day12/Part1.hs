import Data.List (foldl')
import qualified Data.Map.Strict as M

import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

parse :: String -> (String, M.Map String Char)
parse = go1 . lines where
  go1 ((_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:rest):[]:transitions) = (rest, M.fromList (map go2 transitions))
  go1 _ = error "invalid input"

  go2 [l1, l2, c, r1, r2, _, _, _, _, d] = ([l1, l2, c, r1, r2], d)
  go2 _ = error "invalid input"

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
  step n (l10:l20:rest) = step (n-1) (l10 : l20 : go l10 l20 rest) where
    go l2 l1 (c:tape'@(r1:r2:_)) =
      let c' = M.findWithDefault '.' [l2, l1, c, r1, r2] transitions
      in c' : go l1 c tape'
    go _ _ xs = xs
  step _ _ = error "invalid tape"

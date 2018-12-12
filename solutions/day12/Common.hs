module Common where

import qualified Data.Map.Strict as M

parse :: String -> (String, M.Map String Char)
parse = go1 . lines where
  go1 ((_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:rest):[]:transitions) = (rest, M.fromList (map go2 transitions))
  go1 _ = error "invalid input"

  go2 [l1, l2, c, r1, r2, _, _, _, _, d] = ([l1, l2, c, r1, r2], d)
  go2 _ = error "invalid input"

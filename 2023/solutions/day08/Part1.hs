{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as M

import Utils

main :: IO ()
main = mainFor 8 parse (show . solve)

parse :: String -> (String, M.Map String (String, String))
parse = go . lines where
  go (steps:_:ds) = (steps, M.fromList $ map go' ds)
  go' (a:b:c:_:_:_:_:d:e:f:_:_:g:h:i:_) = ([a,b,c], ([d,e,f], [g,h,i]))

solve :: (String, M.Map String (String, String)) -> Int
solve (lrs0, ds0) = go 0 "AAA" lrs0 where
  go !n "ZZZ" _ = n
  go !n pos [] = go n pos lrs0
  go !n pos ('L':lrs) = go (n+1) (fst $ ds0 M.! pos) lrs
  go !n pos ('R':lrs) = go (n+1) (snd $ ds0 M.! pos) lrs

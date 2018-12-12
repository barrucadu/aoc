{-# LANGUAGE BangPatterns #-}

import Common
import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: ([Bool], Transitions) -> Integer
solve (initial, transitions) = calc (step 50000000000 tape0) where
  tape0 =
    let blanks = repeat False
    in (negate (fromIntegral lbuffer), take lbuffer blanks ++ initial ++ blanks)

  lbuffer :: Int
  lbuffer = 3

  calc :: (Integer, [Bool]) -> Integer
  calc (pos, tape) = go 0 pos tape where
    go !acc !n (True:rest) = go (acc + n) (n+1) rest
    go !acc !n (False:rest)
      | empty rest = acc
      | otherwise = go acc (n+1) rest
    go _ _ _ = error "invalid tape"

  step :: Integer -> (Integer, [Bool]) -> (Integer, [Bool])
  step 0 tape = tape
  step n (s, t) = if stable t t' then (s + n, t) else step (n-1) st' where
    st'@(_, t') = resize s (go False False t)

    go False False tape' | empty tape' = tape'
    go l2 l1 (c:tape'@(r1:r2:_)) =
      let c' = findTransition transitions c (toK l2 l1 r1 r2)
      in c' : go l1 c tape'
    go _ _ xs = xs

  resize :: Integer -> [Bool] -> (Integer, [Bool])
  resize s0 t0 = go (s0 - 2) (False:False:t0) where
    go s (False:t@(False:False:_)) = go (s+1) t
    go s t = (s, t)

  stable :: [Bool] -> [Bool] -> Bool
  stable (c:cs) (d:ds) = c == d && (empty cs && empty ds || stable cs ds)
  stable _ _ = False

  empty :: [Bool] -> Bool
  empty = all (==False) . take 30

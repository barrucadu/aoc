{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as M

import Common
import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: (String, M.Map String Char) -> Integer
solve (initial, transitions) = calc (step 50000000000 tape0) where
  tape0 =
    let blanks = repeat '.'
    in (negate (fromIntegral lbuffer), take lbuffer blanks ++ initial ++ blanks)

  lbuffer :: Int
  lbuffer = 3

  calc :: (Integer, String) -> Integer
  calc (pos, tape) = go 0 pos tape where
    go !acc !n ('#':rest) = go (acc + n) (n+1) rest
    go !acc !n ('.':rest)
      | empty rest = acc
      | otherwise = go acc (n+1) rest
    go _ _ _ = error "invalid tape"

  step :: Integer -> (Integer, String) -> (Integer, String)
  step 0 tape = tape
  step n (s, t) = if stable t t' then (s + n, t) else step (n-1) st' where
    st'@(_, t') = resize s (go '.' '.' t)

    go '.' '.' tape' | empty tape' = tape'
    go l2 l1 (c:tape'@(r1:r2:_)) =
      let c' = M.findWithDefault '.' [l2, l1, c, r1, r2] transitions
      in c' : go l1 c tape'
    go _ _ xs = xs

  resize :: Integer -> String -> (Integer, String)
  resize s0 t0 = go (s0 - 2) ('.':'.':t0) where
    go s ('.':t@('.':'.':_)) = go (s+1) t
    go s t = (s, t)

  stable :: String -> String -> Bool
  stable (c:cs) (d:ds) = c == d && (empty cs && empty ds || stable cs ds)
  stable _ _ = False

  empty :: String -> Bool
  empty = all (=='.') . take 30

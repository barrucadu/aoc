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
  step n (s, t) = if stable t t' then (s + n, t) else step (n-1) (trim s t') where
    t' = go '.' '.' t

    go '.' '.' tape' | empty tape' = tape'
    go l2 l1 (c:tape'@(r1:r2:_)) =
      let c' = M.findWithDefault '.' [l2, l1, c, r1, r2] transitions
      in c' : go l1 c tape'
    go _ _ xs = xs

  trim :: Integer -> String -> (Integer, String)
  trim s t
    | empty t = (s+1, tail t)
    | otherwise = (s, t)

  stable :: String -> String -> Bool
  stable t1 t2 = check ('.':t1) t2 where
    check (c:cs) (d:ds) = c == d && (empty cs && empty ds || check cs ds)
    check _ _ = False

  empty :: String -> Bool
  empty = all (=='.') . take 30

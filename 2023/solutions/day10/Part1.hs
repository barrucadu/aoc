{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as M

import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

type P = (Int, Int)

parse :: String -> (P, M.Map P (P, P))
parse = go undefined M.empty 0 0 where
  go s m !_ !_ [] = (s, complete s m)
  go s m !_ !y ('\n':cs) = go s m 0 (y+1) cs
  go s m !x !y ('|':cs) = go s (northsouth x y m) (x+1) y cs
  go s m !x !y ('-':cs) = go s (eastwest x y m) (x+1) y cs
  go s m !x !y ('L':cs) = go s (northeast x y m) (x+1) y cs
  go s m !x !y ('J':cs) = go s (northwest x y m) (x+1) y cs
  go s m !x !y ('7':cs) = go s (southwest x y m) (x+1) y cs
  go s m !x !y ('F':cs) = go s (southeast x y m) (x+1) y cs
  go _ m !x !y ('S':cs) = go (x, y) m (x+1) y cs
  go s m !x !y (_:cs) = go s m (x+1) y cs

  northsouth x y = M.insert (x, y) ((x, y-1), (x, y+1))
  eastwest x y = M.insert (x, y) ((x-1, y), (x+1, y))
  northeast x y = M.insert (x, y) ((x, y-1), (x+1, y))
  northwest x y = M.insert (x, y) ((x, y-1), (x-1, y))
  southwest x y = M.insert (x, y) ((x, y+1), (x-1, y))
  southeast x y = M.insert (x, y) ((x, y+1), (x+1, y))

  complete s@(sx, sy) m = case (connects s (sx-1, sy) m, connects s (sx+1, sy) m, connects s (sx, sy-1) m, connects s (sx, sy+1) m) of
    (False, False, True, True) -> northsouth sx sy m
    (True, True, False, False) -> eastwest sx sy m
    (False, True, True, False) -> northeast sx sy m
    (True, False, True, False) -> northwest sx sy m
    (True, False, False, True) -> southwest sx sy m
    (False, True, False, True) -> southeast sx sy m
    x -> error $ "ambiguous connection " ++ (show (x, s, m))

  connects target start m = case M.lookup start m of
    Just (a, b) -> a == target || b == target
    Nothing -> False

solve :: (P, M.Map P (P, P)) -> Int
solve (s, m) = go 0 s s `div` 2 where
  go !n prior current =
    let (a, b) = m M.! current
        next = if a == prior then b else a
    in if next == s then n+1 else go (n+1) current next

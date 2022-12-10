{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 10 parse solve

type Instr = Maybe Int

parse :: String -> [Instr]
parse = map go . lines where
  go "noop" = Nothing
  go ('a':'d':'d':'x':' ':'-':d) = Just (negate (parseInt d))
  go ('a':'d':'d':'x':' ':d) = Just (parseInt d)

solve :: [Instr] -> String
solve = go [] 1 1 where
  go !acc _ _ [] = reverse (concat acc)
  go !acc !x !cycle (Just d:ds)  = go (draw x (cycle + 1) : draw x cycle : acc) (x + d) (cycle + 2) ds
  go !acc !x !cycle (Nothing:ds) = go (draw x cycle : acc) x (cycle + 1) ds

  draw :: Int -> Int -> String
  draw x cycle =
    let line = if cycle `mod` 40 == 0 then ('\n':) else id
        mark = if ((cycle - 1) `mod` 40) `elem` [x-1, x, x+1] then "#" else "."
    in line mark

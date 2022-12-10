{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

type Instr = Maybe Int

parse :: String -> [Instr]
parse = map go . lines where
  go "noop" = Nothing
  go ('a':'d':'d':'x':' ':'-':d) = Just (negate (parseInt d))
  go ('a':'d':'d':'x':' ':d) = Just (parseInt d)

solve :: [Instr] -> Int
solve = go 0 1 1 where
  go !acc _ _ [] = acc
  go !acc !x !cycle (Just d:ds)  = go (acc + strength x cycle + strength x (cycle + 1)) (x + d) (cycle + 2) ds
  go !acc !x !cycle (Nothing:ds) = go (acc + strength x cycle) x (cycle + 1) ds

  strength x cycle
    | (cycle - 20) `mod` 40 == 0 = x * cycle
    | otherwise = 0

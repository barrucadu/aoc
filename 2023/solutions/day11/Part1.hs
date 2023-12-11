{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 11 parse (show . solve)

solve :: [P] -> Int
solve = solveFor 1

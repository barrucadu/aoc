{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 1 parse (show . solve)

solve :: [Int] -> Int
solve = sum . map (go 0) where
  go !acc m =
    let fuel = fuelForModule m
    in if fuel < 1
       then acc
       else go (acc + fuel) fuel

import qualified Data.Map as M

import           Common
import           Utils

main :: IO ()
main = mainFor 14 parse (show . solve)

solve :: M.Map Ingredient Recipe -> Int
solve recipes = search (error "whoops") 0 maxOre where
  search :: Int -> Int -> Int -> Int
  search sofar lo hi | hi < lo = sofar
  search _ lo hi =
    let mid = (lo + hi) `div` 2
    -- this assumes there's an amount of fuel which uses exactly
    -- `maxOre` ore
    in case compare maxOre (makeFuel recipes mid) of
         LT -> search (mid-1) lo (mid-1)
         EQ -> mid
         GT -> search (mid+1) (mid+1) hi

  maxOre = 1000000000000

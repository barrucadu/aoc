import qualified Data.Map.Strict as M

import Common
import Utils

main :: IO ()
main = mainFor 11 parse (show . solve)

solve :: M.Map Point State -> Int
solve m0 = genericSolve 4 neighbours m0 where
  neighbours = M.mapWithKey neighbours' m0

  neighbours' yx _ = map ($yx) dyxs

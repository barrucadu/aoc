import qualified Data.IntMap.Strict as M

import           Common
import           Utils

main :: IO ()
main = mainFor 19 parse (show . solve)

solve :: (M.IntMap Rule, [String]) -> Int
solve (rules, messages) = length $ filter (checkRule rules' (M.findWithDefault RFail 0 rules')) messages where
  rules' = newRules `M.union` rules

  newRules = M.fromList
    [ (8,  RChoice [[42], [42, 8]])
    , (11, RChoice [[42, 31], [42, 11, 31]])
    ]

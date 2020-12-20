import qualified Data.IntMap.Strict as M

import Common
import Utils

main :: IO ()
main = mainFor 19 parse (show . solve)

solve :: (M.IntMap Rule, [String]) -> Int
solve (rules, messages) = length $ filter (checkRule rules (M.findWithDefault RFail 0 rules)) messages

{-# LANGUAGE BangPatterns #-}

import Common
import Utils

main :: IO ()
main = mainFor 24 parse (show . solve)

solve :: ([Army], [Army]) -> Int
solve (immuneSystem0, infection) = search 1 where
  search !n = case fight (map (boost n) immuneSystem0, infection) of
    Just (score, ImmuneSystem) -> score
    _ -> search (n+1)

  boost n army = army { unitDamage = unitDamage army + n }

{-# LANGUAGE NamedFieldPuns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: [Planet] -> Int
solve = go 0 where
  go n planets =
    let planets' = step planets
        n' = n + 1
    in if n' == target
       then sum (map energy planets')
       else go n' planets'

  step planets = map (step' planets) planets

  step' planets0 me =
    let vx' = vx me + sum [delta px me other | other <- planets0 ]
        vy' = vy me + sum [delta py me other | other <- planets0 ]
        vz' = vz me + sum [delta pz me other | other <- planets0 ]
    in Planet
       { px = px me + vx'
       , py = py me + vy'
       , pz = pz me + vz'
       , vx = vx'
       , vy = vy'
       , vz = vz'
       }

energy :: Planet -> Int
energy Planet {px, py, pz, vx, vy, vz} =
  let potential = abs px + abs py + abs pz
      kinetic = abs vx + abs vy + abs vz
  in potential * kinetic

target :: Int
target = 1000

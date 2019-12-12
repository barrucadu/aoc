{-# LANGUAGE NamedFieldPuns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: [Planet] -> Int
solve planets = lcm xcyclelen (lcm ycyclelen zcyclelen) where
  xcyclelen = cyclelen px vx (\v me -> me { px = px me + v, vx = v }) planets
  ycyclelen = cyclelen py vy (\v me -> me { py = py me + v, vy = v }) planets
  zcyclelen = cyclelen pz vz (\v me -> me { pz = pz me + v, vz = v }) planets

cyclelen
  :: (Planet -> Int)
  -- ^ Get position
  -> (Planet -> Int)
  -- ^ Get velocity
  -> (Int -> Planet -> Planet)
  -- ^ Set velocity and update position
  -> [Planet]
  -> Int
cyclelen getPos getVel setPosVel planets0 = go 0 planets0 where
  go n planets =
    let planets' = step planets
        n' = n + 1
    in if map getPos planets0 == map getPos planets'
       then n' + 1 -- why does this +1 make it work?
       else go n' planets'

  step planets = map (step' planets) planets

  step' others me =
    let v' = getVel me + sum [delta getPos me other | other <- others ]
    in setPosVel v' me

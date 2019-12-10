import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Common
import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

solve :: S.Set (Int, Int) -> Int
solve asteroids = maximum
  [ M.size (asteroidsByAngle xy asteroids)
  | xy <- S.toList asteroids
  ]

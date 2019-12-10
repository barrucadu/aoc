import qualified Data.Set as S

import           Common
import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

solve :: S.Set (Int, Int) -> Int
solve asteroids = maximum . map (length . visibleAsteroids) $ S.toList asteroids where
  visibleAsteroids xy0 = [xy | xy <- S.toList asteroids, null (asteroidsBetween asteroids xy0 xy), xy /= xy0]

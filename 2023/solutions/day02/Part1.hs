import           Common
import           Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

solve :: [(Int, [RGB])] -> Int
solve = sum . map fst . filter check where
  check (_, configs) = all check' configs
  check' (RGB r g b) = r <= 12 && g <= 13 && b <= 14

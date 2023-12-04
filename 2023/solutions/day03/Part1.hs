import qualified Data.Set as S

import           Common
import           Utils

main :: IO ()
main = mainFor 3 (parse (const True)) (show . solve)

solve :: ([Token], S.Set Point) -> Int
solve (tokens, points) = sum . map tValue $ filter check tokens where
  check t = not $ tNeighbours t `S.disjoint` points

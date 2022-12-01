import qualified Data.Set as S

import           Common
import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: (S.Set Point, [Fold]) -> Int
solve (ps0, (f:_)) = S.size (fold f ps0)
solve _ = error "no folds"

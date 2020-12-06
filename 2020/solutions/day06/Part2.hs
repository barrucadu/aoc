import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 6 (parse S.intersection) (show . solve)

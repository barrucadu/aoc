import           Control.Arrow (second)

import           Common
import           Utils

main :: IO ()
main = mainFor 9 (second (*100) . parse) (show . solve)

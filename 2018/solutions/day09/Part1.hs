import           Common
import           Utils

main :: IO ()
main = mainFor 9 parse (show . solve)

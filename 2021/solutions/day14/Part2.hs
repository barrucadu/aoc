import           Common
import           Utils

main :: IO ()
main = mainFor 14 parse (show . solve 40)

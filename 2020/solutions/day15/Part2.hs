import           Common
import           Utils

main :: IO ()
main = mainFor 15 parse (show . solve 30000000)

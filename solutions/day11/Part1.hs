import Common
import Utils

main :: IO ()
main = mainFor 11 (parseInt . init) (show . solve [3])

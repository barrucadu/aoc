import           Common
import           Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

solve :: ([Int], [BingoState]) -> Int
solve = uncurry (playBingo True)

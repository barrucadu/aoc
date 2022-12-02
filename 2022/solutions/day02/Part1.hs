import           Common
import           Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

parse :: String -> [(RPS, RPS)]
parse = map go . lines where
  go [theirs, _, mine] =
    let rps1 = case theirs of 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors
        rps2 = case mine of 'X' -> Rock; 'Y' -> Paper; 'Z' -> Scissors
    in (rps1, rps2)

solve :: [(RPS, RPS)] -> Int
solve = sum . map score where
  score (theirs, mine)
    | mine `beats` theirs = 6 + throwScore mine
    | mine == theirs = 3 + throwScore mine
    | otherwise = throwScore mine

import           Common
import           Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

data LTW = Lose | Tie | Win
  deriving Eq

parse :: String -> [(RPS, LTW)]
parse = map go . lines where
  go [theirs, _, mine] =
    let rps = case theirs of 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors
        mode = case mine of 'X' -> Lose; 'Y' -> Tie; 'Z' -> Win
    in (rps, mode)

solve :: [(RPS, LTW)] -> Int
solve = sum . map score where
  score (rps, Lose) = throwScore (loser rps)
  score (rps, Tie) = 3 + throwScore rps
  score (rps, Win) = 6 + throwScore (victor rps)

import           Data.Char (isDigit)

import           Utils

main :: IO ()
main = mainFor 1 solve show

solve :: String -> Int
solve = sum . map go . lines where
  go cs =
    let digits = map parseDigit (filter isDigit cs)
    in head digits * 10 + last digits

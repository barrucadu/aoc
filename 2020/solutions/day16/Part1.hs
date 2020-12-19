import Common
import Utils

main :: IO ()
main = mainFor 16 parse (show . solve)

solve :: ([Rule], a, [Ticket]) -> Int
solve (rules, _, tickets) = sum $ map go tickets where
  go ticket = sum [ n | n <- ticket, invalid n ]
  invalid n = not $ any (checkRule n) rules

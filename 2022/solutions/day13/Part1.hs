import           Common
import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: Ord a => [(a, a)] -> Int
solve lrs = sum [n | (n, (l, r)) <- zip [1..] lrs, l <= r]

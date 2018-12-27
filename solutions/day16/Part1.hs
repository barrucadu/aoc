import           Common
import           Utils

main :: IO ()
main = mainFor 16 parse (show . solve)

solve :: ([Example], a) -> Int
solve = length . filter (\ops -> length ops >= 3) . map compatabilities . fst where
  compatabilities e = filter (compatible e) [minBound..maxBound]

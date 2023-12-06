import           Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

data Race = Race { rTime :: Int, rDistance :: Int }
  deriving Show

parse :: String -> [Race]
parse input =
  let [times, distances] = lines input
      numbers = map parseInt . tail . words
  in zipWith Race (numbers times) (numbers distances)

solve :: [Race] -> Int
solve = product . map go where
  go r =
    let charging_times = [0..rTime r]
    in 1 + find (reverse charging_times) r - find charging_times r

  find ts (Race time distance) = head $ [charging_time | charging_time <- ts, charging_time * (time - charging_time) > distance]

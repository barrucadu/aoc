import           Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

data Race = Race { rTime :: Int, rDistance :: Int }
  deriving Show

parse :: String -> Race
parse input =
  let [times, distances] = lines input
      numbers = parseInt . concat . tail . words
  in Race (numbers times) (numbers distances)

solve :: Race -> Int
solve (Race time distance) = 1 + hi - lo where
  ftime = fromIntegral time :: Double

  x = sqrt $ ftime ** 2 - 4 * fromIntegral distance

  lo =
    let lo' = ceiling $ (ftime - x) / 2
    in head $ [charging_time | dl <- [-1, 0], let charging_time = lo' + dl, check charging_time]

  hi =
    let hi' = floor $ (ftime + x) / 2
    in head $ [charging_time | dl <- [0, 1], let charging_time = hi' + dl, check charging_time]

  check charging_time = charging_time * (time - charging_time) > distance

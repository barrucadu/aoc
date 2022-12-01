{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

parse :: String -> (Int, [Int])
parse = go . lines where
  go [ts, ids] = (parseInt ts, go' 0 ids)
  go _ = error "invalid input"

  go' !acc (',':cs)
    | acc == 0  = go' 0 cs
    | otherwise = acc : go' 0 cs
  go' !acc ('x':cs) = go' acc cs
  go' !acc (c:cs)   = go' (stepParseInt acc c) cs
  go' !acc [] = [acc]

solve :: (Int, [Int]) -> Int
solve (ts, ids) = uncurry (*) . minimum $ map waitFor ids where
  waitFor bId = waitFor' bId where
    waitFor' !bTs
      | bTs >= ts = (bTs - ts, bId)
      | otherwise = waitFor' (bTs + bId)

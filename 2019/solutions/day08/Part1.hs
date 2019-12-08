{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 8 parse (show . solve)

parse :: String -> [(Int, Int, Int)]
parse = go layerSize 0 0 0 where
  go 0 !zeroes !ones !twos rest = (zeroes, ones, twos) : go layerSize 0 0 0 rest
  go !n !zeroes !ones !twos ('0':rest) = go (n-1) (zeroes+1) ones twos rest
  go !n !zeroes !ones !twos ('1':rest) = go (n-1) zeroes (ones+1) twos rest
  go !n !zeroes !ones !twos ('2':rest) = go (n-1) zeroes ones (twos+1) rest
  go _ _ _ _ ['\n'] = []
  go _ _ _ _ rest = error ("unexpected input: " ++ rest)

  layerSize = 25 * 6 :: Int

solve :: [(Int, Int, Int)] -> Int
solve = (\(_, ones, twos) -> ones * twos) . minimum

{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 3 parse (show . solve)

parse :: String -> [[Bool]]
parse = map (map (=='1')) . lines

solve :: [[Bool]] -> Int
solve nums =
  let oxygenGeneratorRating = bitsToInt (findRatingBits (>=) nums)
      co2ScrubberRating = bitsToInt (findRatingBits (<) nums)
  in oxygenGeneratorRating * co2ScrubberRating

findRatingBits :: (Int -> Int -> Bool) -> [[Bool]] -> [Bool]
{-# INLINE findRatingBits #-}
findRatingBits f = go 0 where
  go :: Int -> [[Bool]] -> [Bool]
  go _ [bits] = bits
  go !i nums =
    let criterion = selectBit (map (!! i) nums)
        nums' = filter (\bits -> bits !! i == criterion) nums
    in go (i+1) nums'

  selectBit :: [Bool] -> Bool
  selectBit [b] = b
  selectBit bs0 = selectBit' 0 0 bs0 where
    selectBit' :: Int -> Int -> [Bool] -> Bool
    selectBit' !ones !zeroes (True:bs) = selectBit' (ones+1) zeroes bs
    selectBit' !ones !zeroes (False:bs) = selectBit' ones (zeroes+1) bs
    selectBit' !ones !zeroes []
      | f ones zeroes = True
      | otherwise = False

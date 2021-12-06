{-# LANGUAGE BangPatterns #-}

module Common where

import Utils (stepParseInt)

parse :: String -> [Int]
parse = go . lines where
  go :: [String] -> [Int]
  go [l] = go' 0 l
  go _ = error "invalid input"

  go' :: Int -> String -> [Int]
  go' !n [] = [n]
  go' !n (',':rest) = n : go' 0 rest
  go' !n (c:rest) = go' (stepParseInt n c) rest

type FishCounter = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

countFishAtDay :: Int -> [Int] -> Int
countFishAtDay days = count . runDays days . makeCounter where
  makeCounter :: [Int] -> FishCounter
  makeCounter fish =
    ( length . filter (== 0) $ fish
    , length . filter (== 1) $ fish
    , length . filter (== 2) $ fish
    , length . filter (== 3) $ fish
    , length . filter (== 4) $ fish
    , length . filter (== 5) $ fish
    , length . filter (== 6) $ fish
    , length . filter (== 7) $ fish
    , length . filter (== 8) $ fish
    )

  runDays :: Int -> FishCounter -> FishCounter
  runDays 0 fish = fish
  runDays !n fish = runDays (n-1) (step fish)

  step :: FishCounter -> FishCounter
  step (today, inOneDay, inTwoDays, inThreeDays, inFourDays, inFiveDays, inSixDays, inSevenDays, inEightDays) =
    (inOneDay, inTwoDays, inThreeDays, inFourDays, inFiveDays, inSixDays, inSevenDays+today, inEightDays, today)

  count :: FishCounter -> Int
  count (today, inOneDay, inTwoDays, inThreeDays, inFourDays, inFiveDays, inSixDays, inSevenDays, inEightDays) =
    today + inOneDay + inTwoDays + inThreeDays + inFourDays + inFiveDays + inSixDays + inSevenDays + inEightDays

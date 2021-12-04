{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.IntMap.Strict as M

import Utils (stepParseInt)

-- this makes me sad, but it's faster than something like
-- `numUnmarkedInColumn :: M.IntMap Int`
data BingoState = BingoState
  { numUnmarkedInColumn0 :: !Int
  , numUnmarkedInColumn1 :: !Int
  , numUnmarkedInColumn2 :: !Int
  , numUnmarkedInColumn3 :: !Int
  , numUnmarkedInColumn4 :: !Int
  , numUnmarkedInRow0 :: !Int
  , numUnmarkedInRow1 :: !Int
  , numUnmarkedInRow2 :: !Int
  , numUnmarkedInRow3 :: !Int
  , numUnmarkedInRow4 :: !Int
  , unmarked :: M.IntMap (Int, Int)
  } deriving (Eq, Ord, Read, Show)

parse :: String -> ([Int], [BingoState])
parse = go . lines where
  go (nums:boards) = (parseNums nums, parseBoards boards)
  go _ = error "invalid input"

  parseNums :: String -> [Int]
  parseNums = parseNums' 0 where
    parseNums' !n [] = [n]
    parseNums' !n (',':rest) = n : parseNums' 0 rest
    parseNums' !n (c:rest) = parseNums' (stepParseInt n c) rest

  parseBoards :: [String] -> [BingoState]
  parseBoards [] = []
  parseBoards ([]:r1:r2:r3:r4:r5:rest) = parseBoard r1 r2 r3 r4 r5 : parseBoards rest
  parseBoards _ = error "invalid input"

  parseBoard :: String -> String -> String -> String -> String -> BingoState
  parseBoard r1 r2 r3 r4 r5 = BingoState
    { numUnmarkedInColumn0 = 5
    , numUnmarkedInColumn1 = 5
    , numUnmarkedInColumn2 = 5
    , numUnmarkedInColumn3 = 5
    , numUnmarkedInColumn4 = 5
    , numUnmarkedInRow0 = 5
    , numUnmarkedInRow1 = 5
    , numUnmarkedInRow2 = 5
    , numUnmarkedInRow3 = 5
    , numUnmarkedInRow4 = 5
    , unmarked = M.unions [parseBoardRow 0 r1, parseBoardRow 1 r2, parseBoardRow 2 r3, parseBoardRow 3 r4, parseBoardRow 4 r5]
    }
    where
      parseBoardRow :: Int -> String -> M.IntMap (Int, Int)
      parseBoardRow y = parseBoardRow' 0 0 . dropWhile (==' ') where
        parseBoardRow' :: Int -> Int -> String -> M.IntMap (Int, Int)
        parseBoardRow' !x !n [] = M.singleton n (x, y)
        parseBoardRow' !x !n (' ':rest) = M.insert n (x, y) $ parseBoardRow' (x+1) 0 (dropWhile (==' ') rest)
        parseBoardRow' !x !n (c:rest) = parseBoardRow' x (stepParseInt n c) rest

playBingo :: Bool -> [Int] -> [BingoState] -> Int
playBingo first = go where
  go [] = error "no solution"
  go (n:ns) = bingo [] where
    bingo bs' [] = go ns bs'
    bingo bs' (b:bs) = case M.lookup n (unmarked b) of
      Just (x, y) ->
        let b' = BingoState
              { numUnmarkedInColumn0 = if x == 0 then numUnmarkedInColumn0 b - 1 else numUnmarkedInColumn0 b
              , numUnmarkedInColumn1 = if x == 1 then numUnmarkedInColumn1 b - 1 else numUnmarkedInColumn1 b
              , numUnmarkedInColumn2 = if x == 2 then numUnmarkedInColumn2 b - 1 else numUnmarkedInColumn2 b
              , numUnmarkedInColumn3 = if x == 3 then numUnmarkedInColumn3 b - 1 else numUnmarkedInColumn3 b
              , numUnmarkedInColumn4 = if x == 4 then numUnmarkedInColumn4 b - 1 else numUnmarkedInColumn4 b
              , numUnmarkedInRow0 = if y == 0 then numUnmarkedInRow0 b - 1 else numUnmarkedInRow0 b
              , numUnmarkedInRow1 = if y == 1 then numUnmarkedInRow1 b - 1 else numUnmarkedInRow1 b
              , numUnmarkedInRow2 = if y == 2 then numUnmarkedInRow2 b - 1 else numUnmarkedInRow2 b
              , numUnmarkedInRow3 = if y == 3 then numUnmarkedInRow3 b - 1 else numUnmarkedInRow3 b
              , numUnmarkedInRow4 = if y == 4 then numUnmarkedInRow4 b - 1 else numUnmarkedInRow4 b
              , unmarked = M.delete n (unmarked b)
              }
        in if gameOver b'
           then
             if first || (null bs' && null bs)
             then score b'
             else bingo bs' bs
           else bingo (b':bs') bs
      Nothing -> bingo (b:bs') bs

    gameOver b =
      numUnmarkedInColumn0 b == 0 ||
      numUnmarkedInColumn1 b == 0 ||
      numUnmarkedInColumn2 b == 0 ||
      numUnmarkedInColumn3 b == 0 ||
      numUnmarkedInColumn4 b == 0 ||
      numUnmarkedInRow0 b == 0 ||
      numUnmarkedInRow1 b == 0 ||
      numUnmarkedInRow2 b == 0 ||
      numUnmarkedInRow3 b == 0 ||
      numUnmarkedInRow4 b == 0

    score b = sum (M.keys (unmarked b)) * n

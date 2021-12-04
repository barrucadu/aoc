{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.IntMap.Strict as M

import Utils (stepParseInt)

data BingoState = BingoState
  { numUnmarkedInColumn :: M.IntMap Int
  , numUnmarkedInRow :: M.IntMap Int
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
    { numUnmarkedInColumn = M.fromAscList [(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)]
    , numUnmarkedInRow = M.fromAscList [(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)]
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
              { numUnmarkedInColumn = M.adjust (subtract 1) x (numUnmarkedInColumn b)
              , numUnmarkedInRow = M.adjust (subtract 1) y (numUnmarkedInRow b)
              , unmarked = M.delete n (unmarked b)
              }
        in if M.lookup x (numUnmarkedInColumn b') == Just 0 || M.lookup y (numUnmarkedInRow b') == Just 0
           then
             if first || (null bs' && null bs)
             then score b'
             else bingo bs' bs
           else bingo (b':bs') bs
      Nothing -> bingo (b:bs') bs

    score b = sum (M.keys (unmarked b)) * n

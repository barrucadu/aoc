{-# LANGUAGE BangPatterns #-}

import Utils

import qualified Data.Map as M

data Point = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Read, Show)

main :: IO ()
main = mainFor 5 parse (show . solve)

parse :: String -> M.Map Point Bool
parse = M.unionsWith (\_ _ -> True) . map go . lines where
  go = goX1 0

  goX1 !x1 (',':rest) = goY1 x1 0 rest
  goX1 !x1 (d:rest) = goX1 (stepParseInt x1 d) rest
  goX1 _ _ = error "invalid input"

  goY1 !x1 !y1 (' ':'-':'>':' ':rest) = goX2 x1 y1 0 rest
  goY1 !x1 !y1 (d:rest) = goY1 x1 (stepParseInt y1 d) rest
  goY1 _ _ _ = error "invalid input"

  goX2 !x1 !y1 !x2 (',':rest) = goY2 x1 y1 x2 0 rest
  goX2 !x1 !y1 !x2 (d:rest) = goX2 x1 y1 (stepParseInt x2 d) rest
  goX2 _ _ _ _ = error "invalid input"

  goY2 !x1 !y1 !x2 !y2 []
    | x1 == x2 = M.fromList [(P x1 y, False) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = M.fromList [(P x y1, False) | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = M.empty
  goY2 !x1 !y1 !x2 !y2 (d:rest) = goY2 x1 y1 x2 (stepParseInt y2 d) rest

solve :: M.Map a Bool -> Int
solve = M.size . M.filter id

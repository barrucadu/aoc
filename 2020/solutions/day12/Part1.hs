{-# LANGUAGE BangPatterns #-}

import Common
import Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: [Instr] -> Int
solve is0 = abs x' + abs y' where
  P x' y' = go FE (P 0 0) is0

  go :: Facing -> Point -> [Instr] -> Point
  go !fc !p (i:is) = go' fc p i is
  go !_  !p [] = p

  go' :: Facing -> Point -> Instr -> [Instr] -> Point
  go' fc p (N d) = go fc (shift FN d p)
  go' fc p (S d) = go fc (shift FS d p)
  go' fc p (E d) = go fc (shift FE d p)
  go' fc p (W d) = go fc (shift FW d p)
  go' fc p (L a) = go (turnL a fc) p
  go' fc p (R a) = go (turnR a fc) p
  go' fc p (F d) = go fc (shift fc d p)

  turnL :: Int -> Facing -> Facing
  turnL !0 fc = fc
  turnL !a FN = turnL (a-90) FW
  turnL !a FS = turnL (a-90) FE
  turnL !a FE = turnL (a-90) FN
  turnL !a FW = turnL (a-90) FS

  turnR :: Int -> Facing -> Facing
  turnR !0 fc = fc
  turnR !a FN = turnR (a-90) FE
  turnR !a FS = turnR (a-90) FW
  turnR !a FE = turnR (a-90) FS
  turnR !a FW = turnR (a-90) FN

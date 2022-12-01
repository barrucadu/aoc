{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 12 parse (show . solve)

solve :: [Instr] -> Int
solve is0 = abs x' + abs y' where
  P x' y' = go (P 0 0) (P 10 (-1)) is0

  go :: Point -> Point -> [Instr] -> Point
  go !pS !pW (i:is) = go' pS pW i is
  go !pS !_ [] = pS

  go' :: Point -> Point -> Instr -> [Instr] -> Point
  go' pS pW (N d) = go pS (shift FN d pW)
  go' pS pW (S d) = go pS (shift FS d pW)
  go' pS pW (E d) = go pS (shift FE d pW)
  go' pS pW (W d) = go pS (shift FW d pW)
  go' pS pW (L a) = go pS (turnL a pW)
  go' pS pW (R a) = go pS (turnR a pW)
  go' (P xS yS) pW@(P xW yW) (F d) = go (P (xS + xW * d) (yS + yW * d)) pW

  turnL :: Int -> Point -> Point
  turnL a0 (P xT0 yT0)= turnL' a0 xT0 yT0 where
    turnL' !0 !x !y = P x y
    turnL' !a !x !y = turnL' (a-90) y (-x)

  turnR :: Int -> Point -> Point
  turnR a0 (P xT0 yT0)= turnR' a0 xT0 yT0 where
    turnR' !0 !x !y = P x y
    turnR' !a !x !y = turnR' (a-90) (-y) x

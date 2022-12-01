{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 18 parse (show . solve)

solve :: [Expr] -> Int
solve = sum . map evalE where
  evalE = evalE2 . evalE1 where
    evalE1 (a0:rest0) = evalE1' (evalA a0) rest0 where
      evalE1' !acc (AOpAdd:a:rest) = evalE1' (acc + evalA a) rest
      evalE1' !acc (AOpMul:a:rest) = ANum acc : AOpMul : evalE1' (evalA a) rest
      evalE1' !acc [] = [ANum acc]
      evalE1' _ _ = error "invalid input"
    evalE1 [] = [ANum 0]

    evalE2 (a0:rest0) = evalE2' (evalA a0) rest0 where
      evalE2' !_ (AOpAdd:_) = error "invariant broken - unexpected AOpAdd"
      evalE2' !acc (AOpMul:a:rest) = evalE2' (acc * evalA a) rest
      evalE2' !acc [] = acc
      evalE2' _ _ = error "invalid input"
    evalE2 [] = 0

  evalA (ANum n) = n
  evalA (ANested e) = evalE e
  evalA _ = error "invalid input"

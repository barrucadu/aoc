{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 18 parse (show . solve)

solve :: [Expr] -> Int
solve = sum . map evalE where
  evalE (a0:rest0) = evalE' (evalA a0) rest0 where
    evalE' !acc (AOpAdd:a:rest) = evalE' (acc + evalA a) rest
    evalE' !acc (AOpMul:a:rest) = evalE' (acc * evalA a) rest
    evalE' !acc [] = acc
    evalE' _ _ = error "invalid input"
  evalE [] = 0

  evalA (ANum n) = n
  evalA (ANested e) = evalE e
  evalA _ = error "invalid input"

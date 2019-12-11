{-# LANGUAGE LambdaCase #-}

module Common where

import           Control.Monad.ST (runST)
import qualified Data.Map.Strict  as M

import           Intcode

data Dir = U | R | D | L

runPainter :: Int -> Program -> M.Map (Int, Int) Int
{-# INLINABLE runPainter #-}
runPainter start program = runST $ do
    mem <- initialise program
    goRead (runPartial mem) U (0, 0) (M.singleton (0, 0) start)
  where
    goRead k dir xy paint = k >>= \case
      In k' m -> m (M.findWithDefault 0 xy paint) >> goPaint k' dir xy paint
      Out _ _ -> error "unexpected Out instruction in goRead"
      Stop -> pure paint

    goPaint k dir xy paint = k >>= \case
      In _ _ -> error "unexpected In instruction in goPaint"
      Out k' a -> goTurn k' dir xy (M.insert xy a paint)
      Stop -> pure paint

    goTurn k dir xy paint = k >>= \case
      In _ _ -> error "unexpected In instruction in goTurn"
      Out k' a ->
        let dir' = (if a == 0 then turnLeft else turnRight) dir
            xy' = step dir' xy
        in goRead k' dir' xy' paint
      Stop -> pure paint

turnLeft :: Dir -> Dir
turnLeft U = L
turnLeft R = U
turnLeft D = R
turnLeft L = D

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

step :: Dir -> (Int, Int) -> (Int, Int)
step U (x, y) = (x, y - 1)
step R (x, y) = (x + 1, y)
step D (x, y) = (x, y + 1)
step L (x, y) = (x - 1, y)

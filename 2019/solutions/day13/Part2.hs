{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Intcode
import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: Program -> Int
solve program = runST $ do
    mem <- VUM.new 4096
    initialise' mem program
    VUM.unsafeWrite mem 0 2
    play (error "no score") (error "no paddle X") (error "no ball X") (runPartial mem)
  where
    play score paddleX ballX k = go score paddleX ballX k >>= \case
      Right (m, score', paddleX', ballX', k') ->
        let dir = if | paddleX' < ballX' -> 1
                     | paddleX' > ballX' -> -1
                     | otherwise -> 0
        in m dir >> play score' paddleX' ballX' k'
      Left score' -> pure score'

    go score paddleX ballX k = k >>= \case
      In k' m -> pure $ Right (m, score, paddleX, ballX, k')
      Out k' x -> k' >>= \case
        Out k'' y -> k'' >>= \case
          Out k''' 3 -> go score x ballX k'''
          Out k''' 4 -> go score paddleX x k'''
          Out k''' a
            | x == -1 && y == 0 -> go a paddleX ballX k'''
            | otherwise -> go score paddleX ballX k'''
          In _ _ -> error "unexpected In instruction in Out/Out"
          Stop -> error "unexpected Stop instruction in Out/Out"
        In _ _ -> error "unexpected In instruction in Out"
        Stop -> error "unexpected Stop instruction in Out"
      Stop -> pure (Left score)

{-# LANGUAGE LambdaCase #-}

import           Control.Monad.ST            (runST)
import qualified Data.Map.Strict             as M
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Intcode
import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: Program -> Int
solve program = runST $ do
    mem <- VUM.new 4096
    initialise' mem program
    tiles <- goX (runPartial mem) M.empty
    pure . length $ filter (==2) (M.elems tiles)
  where
    goX k tiles = k >>= \case
      In _ _ -> error "unexpected In instruction in goX"
      Out k' x -> goY x k' tiles
      Stop -> pure tiles

    goY x k tiles = k >>= \case
      In _ _ -> error "unexpected In instruction in goY"
      Out k' y -> goDraw x y k' tiles
      Stop -> pure tiles

    goDraw x y k tiles = k >>= \case
      In _ _ -> error "unexpected In instruction in goDraw"
      Out k' a -> goX k' (M.insert (x, y) a tiles)
      Stop -> pure tiles

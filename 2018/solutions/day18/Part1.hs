{-# LANGUAGE BangPatterns #-}

import           Control.Monad.ST (ST, runST)

import           Common
import           Utils

main :: IO ()
main = mainFor 18 id (show . solve)

solve :: String -> Int
solve input = runST $ do
    let dim@(w, h) = getWH input
    sCur  <- newArray w h
    sNext <- newArray w h
    parse sCur input
    score dim =<< solve' dim 10 sCur sNext
  where
    solve' :: Dimensions -> Int -> Automata s -> Automata s -> ST s (Automata s)
    solve' dim = go where
      go  0 sCur _ = pure sCur
      go !n sCur sNext = do
        step dim sCur sNext
        go (n-1) sNext sCur

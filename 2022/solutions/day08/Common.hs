module Common where

import           Data.Foldable (for_)

import           Utils

parse :: String -> Array Int
parse input = createArray $ do
  let ls = lines input
  let height = length ls
  let width = length (head ls)
  arr <- newArray width height
  for_ (zip [0..] ls) $ \(y, l) ->
    for_ (zip [0..] l) $ \(x, c) ->
      writeArray arr x y (parseDigit c)
  pure arr

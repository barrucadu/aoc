{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 2 parse (show . solve)

data RGB = RGB !Int !Int !Int

parse :: String -> [RGB]
parse = map (go . words) . lines where
  go (_:_:configs) = minCubes (RGB 0 0 0) configs

  minCubes !(RGB r g b) (num:color:rest) =
    let rgb' = case head color of
          'r' -> RGB (max r $ parseInt num) g b
          'g' -> RGB r (max g $ parseInt num) b
          'b' -> RGB r g (max b $ parseInt num)
    in minCubes rgb' rest
  minCubes !rgb _ = rgb

solve :: [RGB] -> Int
solve = sum . map power where
  power (RGB r g b) = r * b * g

{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils

data RGB = RGB !Int !Int !Int

parse :: String -> [(Int, [RGB])]
parse = map (go . words) . lines where
  go (_:num:configs) = (parseInt (init num), parseConfigs configs)

  parseConfigs [] = []
  parseConfigs l =
    let (config, rest) = parseConfig (RGB 0 0 0) l
    in config : parseConfigs rest

  parseConfig !rgb [] = (rgb, [])
  parseConfig !(RGB r g b) (num:color:rest) =
    let rgb' = case head color of
          'r' -> RGB (parseInt num) g b
          'g' -> RGB r (parseInt num) b
          'b' -> RGB r g (parseInt num)
    in if last color == ';' then (rgb', rest) else parseConfig rgb' rest

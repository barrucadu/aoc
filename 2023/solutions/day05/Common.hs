module Common where

import           Utils

type Map = [MapRange]

data MapRange = MapRange { mrOut :: Int, mrIn :: Int, mrLen :: Int }
  deriving Show

parse :: String -> ([Int], [Map])
parse = (go . map words) . lines where
  go ((_:seeds):_:ls) = (map parseInt seeds, goMaps ls)

  goMaps (_:ls) = let (m, ls') = goMap [] ls in m : goMaps ls'
  goMaps [] = []

  goMap ms ([o,i,r]:ls) = goMap (toMapRange o i r:ms) ls
  goMap ms ([]:ls) = (ms, ls)
  goMap ms [] = (ms, [])

  toMapRange o i r = MapRange
    { mrOut = parseInt o
    , mrIn = parseInt i
    , mrLen = parseInt r
    }

mrEnd :: MapRange -> Int
mrEnd m = mrIn m + mrLen m

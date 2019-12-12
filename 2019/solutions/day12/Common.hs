module Common where

import           Data.Ord (comparing)

import           Utils

data Planet = Planet
  { px :: {-# UNPACK #-} !Int
  , py :: {-# UNPACK #-} !Int
  , pz :: {-# UNPACK #-} !Int
  , vx :: {-# UNPACK #-} !Int
  , vy :: {-# UNPACK #-} !Int
  , vz :: {-# UNPACK #-} !Int
  }

parse :: String -> [Planet]
{-# INLINABLE parse #-}
parse = map go . lines where
  go ('<':rest) =
    let (px, ' ':rest') = goInt rest
        (py, ' ':rest'') = goInt rest'
        (pz, _) = goInt rest''
    in Planet px py pz 0 0 0
  go x = error $ "bad input: " ++ x

  goInt (_:_:'-':rest) =
    let (i, rest') = goInt' 0 rest
    in (negate i, rest')
  goInt (_:_:rest) = goInt' 0 rest
  goInt x = error $ "could not parse as int: " ++ x

  goInt' acc (',':rest) = (acc, rest)
  goInt' acc ('>':rest) = (acc, rest)
  goInt' acc (d:rest) = goInt' (stepParseInt acc d) rest
  goInt' acc [] = (acc, [])

delta :: Ord b => (a -> b) -> a -> a -> Int
{-# INLINABLE delta #-}
delta f me other = case comparing f me other of
  GT -> -1
  EQ -> 0
  LT -> 1

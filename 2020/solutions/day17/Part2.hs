import qualified Data.Map.Strict as M

import           Common
import           Utils

main :: IO ()
main = mainFor 17 parse (show . solve)

-------------------------------------------------------------------------------

data Point = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Read, Show)

parse :: String -> M.Map Point State
parse = genericParse (P 0 0)

-------------------------------------------------------------------------------

solve :: M.Map Point State -> Int
solve m0 = genericSolve sn neighbours expanded where
  sn = 6

  expanded = let (P _ _ ymax xmax) = maximum $ M.keys m0 in M.fromList
    [ let p = P w z y x in (p, M.findWithDefault Inactive p m0)
    | w <- [-sn..sn]
    , z <- [-sn..sn]
    , y <- [-sn..ymax+sn]
    , x <- [-sn..xmax+sn]
    ]

  neighbours = M.mapWithKey (\p _ -> map ($p) deltas) expanded

deltas :: [Point -> Point]
deltas =
  [ \(P w z y x) -> P (w+dw) (z+dz) (y+dy) (x+dx)
  | dw <- [-1, 0, 1]
  , dz <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , dx <- [-1, 0, 1]
  , (dw, dz, dy, dx) /= (0, 0, 0, 0)
  ]

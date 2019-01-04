{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.PQueue.Prio.Max as P

import           Common
import           Utils

main :: IO ()
main = mainFor 23 parse (show . solve)

type Bot = ((Int, Int, Int), Int)

data Region = Region
  { rminX :: {-# UNPACK #-} !Int
  , rmaxX :: {-# UNPACK #-} !Int
  , rminY :: {-# UNPACK #-} !Int
  , rmaxY :: {-# UNPACK #-} !Int
  , rminZ :: {-# UNPACK #-} !Int
  , rmaxZ :: {-# UNPACK #-} !Int
  , rbots :: [Bot]
  , rnumbots :: {-# UNPACK #-} !Int
  }
  deriving (Show)

solve :: [Bot] -> Int
solve nanobots = rpartition 0 0 region0 (P.singleton (rnumbots region0) region0) where
  rpartition bestK bestD bestR pq = case P.maxViewWithKey pq of
    Just ((k0, r0), pq')
      | k0 >= bestK ->
        if singleton r0
        then let d0 = manhattan3 (0, 0, 0) (rminX r0, rminY r0, rminZ r0)
             in if | k0 > bestK -> rpartition k0 d0 r0 pq'
                   | d0 < bestD -> rpartition k0 d0 r0 pq'
                   | otherwise  -> rpartition bestK bestD bestR pq'
        else let rs = P.fromList [(rnumbots r, r) | r <- split r0]
             in rpartition bestK bestD bestR (P.union pq' rs)
    _ -> bestD

  region0 =
    -- don't need to consider [minX - r .. maxX + r] (similarly for
    -- other dimensions) as moving beyond [minX .. maxX] can ONLY take
    -- you further away from the nanobots.
    let (minX, maxX) = minmax (map (\((x, _, _), _) -> x) nanobots)
        (minY, maxY) = minmax (map (\((_, y, _), _) -> y) nanobots)
        (minZ, maxZ) = minmax (map (\((_, _, z), _) -> z) nanobots)
        numbots = length nanobots
    in Region
       { rminX = minX
       , rmaxX = maxX
       , rminY = minY
       , rmaxY = maxY
       , rminZ = minZ
       , rmaxZ = maxZ
       , rbots = nanobots
       , rnumbots = numbots
       }

singleton :: Region -> Bool
singleton Region{..} =
  rminX == rmaxX &&
  rminY == rmaxY &&
  rminZ == rmaxZ

split :: Region -> [Region]
split r@Region{..} = map restrictBots $ if
    -- non-minimal along all axes
    | cx && cy && cz ->
      [ r { rmaxX = mx,     rmaxY = my,     rmaxZ = mz }
      , r { rminX = mx + 1, rmaxY = my,     rmaxZ = mz }
      , r { rmaxX = mx,     rminY = my + 1, rmaxZ = mz }
      , r { rminX = mx + 1, rminY = my + 1, rmaxZ = mz }
      , r { rmaxX = mx,     rmaxY = my,     rminZ = mz + 1 }
      , r { rminX = mx + 1, rmaxY = my,     rminZ = mz + 1 }
      , r { rmaxX = mx,     rminY = my + 1, rminZ = mz + 1 }
      , r { rminX = mx + 1, rminY = my + 1, rminZ = mz + 1 }
      ]
    -- non-minimal along two of three axes
    | cx && cy ->
      [ r { rmaxX = mx,     rmaxY = my }
      , r { rminX = mx + 1, rmaxY = my }
      , r { rmaxX = mx,     rminY = my + 1 }
      , r { rminX = mx + 1, rminY = my + 1 }
      ]
    | cx && cz ->
      [ r { rmaxX = mx,     rmaxZ = mz }
      , r { rminX = mx + 1, rmaxZ = mz }
      , r { rmaxX = mx,     rminZ = mz + 1 }
      , r { rminX = mx + 1, rminZ = mz + 1 }
      ]
    | cy && cz ->
      [ r { rmaxY = my,     rmaxZ = mz }
      , r { rminY = my + 1, rmaxZ = mz }
      , r { rmaxY = my,     rminZ = mz + 1 }
      , r { rminY = my + 1, rminZ = mz + 1 }
      ]
    -- non-minimal along only one axis
    | cx -> [r { rmaxX = mx }, r { rminX = mx + 1 }]
    | cy -> [r { rmaxY = my }, r { rminY = my + 1 }]
    | cz -> [r { rmaxZ = mz }, r { rminZ = mz + 1 }]
    -- invalid invocation
    | otherwise -> error "cannot split minimal region"
  where
    cx = rminX /= rmaxX
    cy = rminY /= rmaxY
    cz = rminZ /= rmaxZ

    mx = mid rminX rmaxX
    my = mid rminY rmaxY
    mz = mid rminZ rmaxZ

restrictBots :: Region -> Region
restrictBots r@Region{..} = r { rbots = nanobots, rnumbots = length nanobots } where
  nanobots = filter (inRange r) rbots

inRange :: Region -> Bot -> Bool
inRange Region{..} ((x, y, z), range) = (cx + cy + cz) <= range where
  cx = c rminX rmaxX x
  cy = c rminY rmaxY y
  cz = c rminZ rmaxZ z

  c rmin rmax p
    | p < rmin = rmin - p
    | p > rmax = p - rmax
    | otherwise = 0

mid :: Int -> Int -> Int
mid lo hi = lo + range `div` 2 where
  range = hi - lo

{-# LANGUAGE BangPatterns #-}

import           Common
import           Utils

main :: IO ()
main = mainFor 10 parse (show . solve)

solve :: [((Int, Int), (Int, Int))] -> Int
solve points0 = go t0 where
  go !t
    | yrange t <= 15 = t
    | otherwise = go (t+1)

  funcs = map func points0 where
    func ((x0, y0), (dx, dy)) t = (x0 + t * dx, y0 + t * dy)

  yrange t =
    let points = map ($t) funcs
        (ymin, ymax) = minmax (map snd points)
    in abs (ymax - ymin)

  t0 = 10000 -- looks good from my input

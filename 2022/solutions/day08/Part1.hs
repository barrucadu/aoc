import qualified Data.Set as S

import           Common
import           Utils

main :: IO ()
main = mainFor 8 parse (show . solve)

solve :: Array Int -> Int
solve arr = S.size $ goDown (-1) S.empty 0 0 where
  maxX = widthArray arr - 1
  maxY = heightArray arr - 1

  goDown prior seen x y
    | x > maxX = goUp (-1) seen 0 maxY
    | y > maxY = goDown (-1) seen (x+1) 0
    | otherwise = check (\here seen' -> goDown here seen' x (y+1)) prior seen x y

  goUp prior seen x y
    | x > maxX = goRight (-1) seen 0 0
    | y < 0 = goUp (-1) seen (x+1) maxY
    | otherwise = check (\here seen' -> goUp here seen' x (y-1)) prior seen x y

  goRight prior seen x y
    | y > maxY = goLeft (-1) seen maxX 0
    | x > maxX = goRight (-1) seen 0 (y+1)
    | otherwise = check (\here seen' -> goRight here seen' (x+1) y) prior seen x y

  goLeft prior seen x y
    | y > maxY = seen
    | x < 0 = goLeft (-1) seen maxX (y+1)
    | otherwise = check (\here seen' -> goLeft here seen' (x-1) y) prior seen x y

  check k prior seen x y =
    let here = indexArray arr x y
    in if here > prior
       then k here (S.insert (x, y) seen)
       else k prior seen

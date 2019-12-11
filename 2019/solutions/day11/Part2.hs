import qualified Data.Map.Strict as M

import           Common
import           Intcode
import           Utils

main :: IO ()
main = mainFor 11 parse (unlines . solve)

solve :: Program -> [String]
solve = pp . runPainter 1 where
  pp paint =
    let (minX, maxX) = minmax (map fst (M.keys paint))
        (minY, maxY) = minmax (map snd (M.keys paint))
    in [ [colour paint x y | x <- [minX..maxX]]
       | y <- [minY..maxY]
       ]

  colour paint x y
    | M.findWithDefault 0 (x, y) paint == 0 = ' '
    | otherwise = '#'

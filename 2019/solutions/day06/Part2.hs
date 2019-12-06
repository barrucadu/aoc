import qualified Data.Map.Lazy as M

import           Common
import           Utils

main :: IO ()
main = mainFor 6 parse (show . solve)

solve :: M.Map String (String, Int) -> Int
solve orbits = common where
  (_, youD) = get "YOU"
  (_, sanD) = get "SAN"

  common = go "YOU" where
    go "COM" = youD + sanD
    go k =
      let (parentN, parentD) = get k
      in case M.lookup parentN sanParents of
           Just sanParentD -> youD - parentD + sanD - sanParentD
           Nothing -> go parentN

  sanParents = go M.empty "SAN" where
    go m "COM" = m
    go m k =
      let (parentN, parentD) = get k
      in go (M.insert parentN parentD m) parentN

  get k = M.findWithDefault (error ("missing orbit data for: " ++ k)) k orbits

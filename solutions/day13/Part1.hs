import qualified Data.Map.Strict as M

import           Common
import           Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: (Events, M.Map Pos Cart) -> (Int, Int)
solve (em, pm0) = go pm0 where
  go pm = case step pm of
    Right pm' -> go pm'
    Left (y', x') -> (x', y')

  step = step' M.empty . M.assocs where
    step' pm (c:cs) =
      let (yx, cart) = stepCart em c
      in if checkCollision pm yx cs
         then Left yx
         else step' (M.insert yx cart pm) cs
    step' pm [] = Right pm

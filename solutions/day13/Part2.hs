import qualified Data.Map.Strict as M

import Common
import Utils

main :: IO ()
main = mainFor 13 parse (show . solve)

solve :: (M.Map Pos Event, M.Map Pos Cart) -> (Int, Int)
solve (em, pm0) = go pm0 where
  go pm =
    let pm' = step pm
    in if M.size pm' == 1
       then
         let [(y, x)] = M.keys pm'
         in (x, y)
       else go pm'

  step = step' M.empty . M.assocs where
    step' pm (c:cs) =
      let (yx, cart) = stepCart em c
      in if checkCollision pm yx cs
         then step' (M.delete yx pm) cs
         else step' (M.insert yx cart pm) cs
    step' pm [] = pm

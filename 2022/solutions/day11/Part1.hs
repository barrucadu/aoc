import qualified Data.IntMap.Strict as M
import           Data.List          (foldl', sort)

import           Common
import           Utils

main :: IO ()
main = mainFor 11 parse (show . solve)

solve :: M.IntMap Monkey -> Int
solve = step 20 where
  step 0 ms = go ms
  step n ms = step (n-1) (monkeyBusiness ms)

  go ms =
    let (a:b:_) = reverse . sort . map mInspects $ M.elems ms
    in a * b

monkeyBusiness :: M.IntMap Monkey -> M.IntMap Monkey
monkeyBusiness ms0 = foldl' go ms0 (M.keys ms0) where
  go ms k =
    let m = ms M.! k
        m' = m { mInspects = mInspects m + length (mState m), mState = [] }
        ms' = foldl' (throw m) ms (mState m)
    in M.insert k m' ms'

  throw m ms i =
    let i' = mOp m i `div` 3
        target = if i' `mod` mMod m == 0 then mTrue m else mFalse m
    in M.adjust (\mt -> mt { mState = mState mt ++ [i'] }) target ms

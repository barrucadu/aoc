import qualified Data.IntMap.Strict as M
import qualified Data.Set as S

import Common
import Utils

main :: IO ()
main = mainFor 16 parse (show . solve)

solve :: ([Example], [EncodedInstr]) -> Int
solve (es0, program) = let (r0, _, _, _) = run (infer es0) program in r0 where
  infer :: [Example] -> M.IntMap (S.Set Op)
  infer = go (M.fromList [(i, S.fromList [minBound..maxBound]) | i <- keys]) where
    go known (e@(_, (op, _, _, _), _):es) =
      let opknown = M.findWithDefault undefined op known
      in go (incorporate e known opknown) es
    go known [] = known

    incorporate e@(_, (op, _, _, _), _) known opknown =
      let (k, known') = learn op (compatible e) known opknown
      in k known'

    learn
      :: Int
      -> (Op -> Bool)
      -> M.IntMap (S.Set Op)
      -> S.Set Op
      -> (M.IntMap (S.Set Op) -> M.IntMap (S.Set Op), M.IntMap (S.Set Op))
    learn op p known opknown
      | S.size opknown == 1 = (id, known)
      | otherwise =
        let opknown' = S.filter p opknown
            known' = M.insert op opknown' known
        in if S.size opknown' == 1
           then (\k -> resolve k op (S.elemAt 0 opknown'), known')
           else (id, known')

    resolve known op0 instr = let (k, known') = foldr resolve' (id, M.empty) (M.assocs known) in k known' where
      resolve' (op, opknown) (k, known')
        | op == op0 = (k, M.insert op0 opknown known')
        | otherwise =
          let (k', known'') = learn op (/=instr) (M.insert op opknown known') opknown
          in (k' . k, known'')

  run :: M.IntMap (S.Set Op) -> [EncodedInstr] -> Regs
  run ops = foldl (flip exec) (0, 0, 0, 0) where
    exec e@(op, _, _, _) =
      let instr = S.elemAt 0 (M.findWithDefault undefined op ops)
      in action instr e

  keys = [0..15]

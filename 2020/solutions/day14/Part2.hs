{-# LANGUAGE BangPatterns #-}

import           Data.Bits
import           Data.Word

import qualified Data.Map.Strict as M

import           Utils

main :: IO ()
main = mainFor 14 parse (show . solve)

-------------------------------------------------------------------------------

data Instr
  = Mask (Word64 -> [Word64])
  | Assign !Word64 !Int

parse :: String -> [Instr]
parse = map go . lines where
  go ('m':'a':'s':'k':' ':'=':' ':rest) = goM rest
  go ('m':'e':'m':'[':rest) = goA rest
  go _ = error "invalid input"

  goM = goM' 35 [] zeroBits where
    goM' !i xs ons ('X':cs) = goM' (i-1) (i:xs) ons cs
    goM' !i xs ons ('0':cs) = goM' (i-1) xs ons cs
    goM' !i xs ons ('1':cs) = goM' (i-1) xs (setBit ons i) cs
    goM' !_ xs ons [] = Mask $ \w64 -> explode xs (w64 .|. ons)
    goM' !_ _ _ _ = error "invalid input"

    explode [] w64 = [w64]
    explode (i:is) w64 =
      let w64's = explode is w64
      in map (\w64' -> setBit w64' i) w64's ++ map (\w64' -> clearBit w64' i) w64's

  goA = goA' 0 where
    goA' !acc (']':' ':'=':' ':cs) = Assign (fromIntegral acc) (parseInt cs)
    goA' !acc (c:cs) = goA' (stepParseInt acc c) cs
    goA' !_ [] = error "invalid input"

-------------------------------------------------------------------------------

solve :: [Instr] -> Int
solve = solve' M.empty (:[]) where
  solve' :: M.Map Word64 Int -> (Word64 -> [Word64]) -> [Instr] -> Int
  solve' !mem mf (Assign addr val:is) =
    let writes = M.fromList [(addr', val) | addr' <- mf addr]
    in solve' (writes `M.union` mem) mf is
  solve' !mem _ (Mask mf:is) = solve' mem mf is
  solve' !mem _ [] = sum $ M.elems mem

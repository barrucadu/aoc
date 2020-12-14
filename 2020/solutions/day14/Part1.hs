{-# LANGUAGE BangPatterns #-}

import Data.Bits
import Data.Word

import qualified Data.IntMap.Strict as M

import Utils

main :: IO ()
main = mainFor 14 parse (show . solve)

-------------------------------------------------------------------------------

data Instr
  = Mask (Word64 -> Word64)
  | Assign !Int !Word64

parse :: String -> [Instr]
parse = map go . lines where
  go ('m':'a':'s':'k':' ':'=':' ':rest) = goM rest
  go ('m':'e':'m':'[':rest) = goA rest
  go _ = error "invalid input"

  goM = goM' 35 zeroBits zeroBits where
    goM' !i ons offs ('X':cs) = goM' (i-1) ons offs cs
    goM' !i ons offs ('0':cs) = goM' (i-1) ons (setBit offs i) cs
    goM' !i ons offs ('1':cs) = goM' (i-1) (setBit ons i) offs cs
    goM' !_ ons offs [] = Mask $ \w64 -> (w64 .|. ons) .&. complement offs
    goM' !_ _ _ _ = error "invalid input"

  goA = goA' 0 where
    goA' !acc (']':' ':'=':' ':cs) = Assign acc . fromIntegral $ parseInt cs
    goA' !acc (c:cs) = goA' (stepParseInt acc c) cs
    goA' !_ [] = error "invalid input"

-------------------------------------------------------------------------------

solve :: [Instr] -> Word64
solve = solve' M.empty id where
  solve' :: M.IntMap Word64 -> (Word64 -> Word64) -> [Instr] -> Word64
  solve' !mem mf (Assign addr val:is) = solve' (M.insert addr (mf val) mem) mf is
  solve' !mem _ (Mask mf:is) = solve' mem mf is
  solve' !mem _ [] = sum $ M.elems mem

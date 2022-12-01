{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.IntMap.Strict as M
import qualified Data.Set           as S

import           Utils              (stepParseInt)

data Rule
  = RChar Char
  | RSeq [Int]
  | RChoice [[Int]]
  | RFail
  deriving (Eq, Ord, Read, Show)

parse :: String -> (M.IntMap Rule, [String])
parse = go M.empty . lines where
  go :: M.IntMap Rule -> [String] -> (M.IntMap Rule, [String])
  go !rules ("":ls) = (rules, ls)
  go !rules (l:ls) = go (parseRule rules l) ls
  go !_ [] = error "invalid input"

  parseRule :: M.IntMap Rule -> String -> M.IntMap Rule
  parseRule rules = parseRule1 0 where
    parseRule1 :: Int -> String -> M.IntMap Rule
    parseRule1 !acc (':':' ':rest) = M.insert acc (parseRule2 rest) rules
    parseRule1 !acc (d:rest) = parseRule1 (stepParseInt acc d) rest
    parseRule1 !_ _ = error "invalid input"

    parseRule2 :: String -> Rule
    parseRule2 ['"', c, '"'] = RChar c
    parseRule2 rest = case parseRuleSeq [] 0 rest of
      (ns, []) -> RSeq ns
      (ns, ' ':'|':' ':rest') -> RChoice $ parseRuleChoice [ns] rest'
      _ -> error "invalid input"

    parseRuleSeq :: [Int] -> Int -> String -> ([Int], String)
    parseRuleSeq ns !acc rest@(' ':'|':_) = (reverse $ acc:ns, rest)
    parseRuleSeq ns !acc (' ':rest) = parseRuleSeq (acc:ns) 0 rest
    parseRuleSeq ns !acc (d:rest) = parseRuleSeq ns (stepParseInt acc d) rest
    parseRuleSeq ns !acc [] = (reverse $ acc:ns, [])

    parseRuleChoice :: [[Int]] -> String -> [[Int]]
    parseRuleChoice nss [] = nss
    parseRuleChoice nss rest = case parseRuleSeq [] 0 rest of
      (ns, []) -> (ns:nss)
      (ns, ' ':'|':' ':rest') -> parseRuleChoice (ns:nss) rest'
      _ -> error "invalid input"

checkRule :: M.IntMap Rule -> Rule -> String -> Bool
checkRule rules rule0 s0 = "" `S.member` check rule0 s0 where
  check :: Rule -> String -> S.Set String
  check (RChar c) = checkChar c
  check (RSeq rs) = checkSeq rs
  check (RChoice rss) = checkChoice rss
  check RFail = const S.empty

  checkChar :: Char -> String -> S.Set String
  checkChar c (d:str) | c == d = S.singleton str
  checkChar _ _ = S.empty

  checkSeq :: [Int] -> String -> S.Set String
  checkSeq (r:rs) str = S.unions [ checkSeq rs str' | str' <- S.toList $ check (getRuleByNum r) str ]
  checkSeq [] str = S.singleton str

  checkChoice :: [[Int]] -> String -> S.Set String
  checkChoice rss str = S.unions [ checkSeq rs str | rs <- rss ]

  getRuleByNum :: Int -> Rule
  getRuleByNum r = M.findWithDefault RFail r rules

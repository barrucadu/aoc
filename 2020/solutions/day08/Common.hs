{-# LANGUAGE PatternSynonyms #-}

module Common where

import Data.Word (Word8)

import Utils

type Instruction = (Word8, Int)

pattern Acc = 0
pattern Jmp = 1
pattern Nop = 2

parse :: String -> [Instruction]
parse = map go . lines where
  go ('a':'c':'c':' ':ns) = (Acc, parseNum ns)
  go ('j':'m':'p':' ':ns) = (Jmp, parseNum ns)
  go ('n':'o':'p':' ':ns) = (Nop, parseNum ns)
  go _ = error "invalid input"

  parseNum ('+':ns) = parseInt ns
  parseNum (_:ns) = negate (parseInt ns)
  parseNum _ = error "invalid input"

module Common where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

parse :: String -> M.Map Char (Int, S.Set Char)
parse = go M.empty where
  go g ('S':'t':'e':'p':' ':from:' ':'m':'u':'s':'t':' ':'b':'e':' ':'f':'i':'n':'i':'s':'h':'e':'d':' ':'b':'e':'f':'o':'r':'e':' ':'s':'t':'e':'p':' ':to:' ':'c':'a':'n':' ':'b':'e':'g':'i':'n':'.':'\n':rest) =
    go (M.alter inc to (M.alter (link to) from g)) rest
  go g [] = g
  go _ _ = error "invalid input"

  link to (Just (x, successors)) = Just (x, S.insert to successors)
  link to Nothing = Just (0, S.singleton to)

  inc (Just (x, successors)) = Just (x+1, successors)
  inc Nothing = Just (1, S.empty)

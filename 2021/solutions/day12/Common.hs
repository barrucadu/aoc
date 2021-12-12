module Common where

import Data.Char (isUpper)
import qualified Data.Map as M
import qualified Data.Set as S

type CaveGraph = M.Map Cave (S.Set Cave)

data Cave
  = Start
  | End
  | Big String
  | Small String
  deriving (Eq, Ord, Read, Show)

parse :: String -> CaveGraph
parse = foldr go M.empty . lines where
  go l graph =
    let (from, rest) = parseCave [] l
        (to, []) = parseCave [] rest
    in M.alter (addRoute from) to (M.alter (addRoute to) from graph)

  parseCave o [] = (toCave o, [])
  parseCave o ('-':rest) = (toCave o, rest)
  parseCave o (c:cs) = parseCave (c:o) cs

  toCave "trats" = Start
  toCave "dne" = End
  toCave o
    | all isUpper o = Big o
    | otherwise = Small o

  addRoute Start e = e
  addRoute to (Just exits) = Just (S.insert to exits)
  addRoute to Nothing = Just (S.singleton to)

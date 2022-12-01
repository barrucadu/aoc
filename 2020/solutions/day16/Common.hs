{-# LANGUAGE BangPatterns #-}

module Common where

import           Utils (stepParseInt)

data Rule = Rule
  { rName :: String
  , rRange1 :: (Int, Int)
  , rRange2 :: (Int, Int)
  }
  deriving (Eq, Ord, Read, Show)

type Ticket = [Int]

parse :: String -> ([Rule], Ticket, [Ticket])
parse = goR [] . lines where
  goR :: [Rule] -> [String] -> ([Rule], Ticket, [Ticket])
  goR rs ("":_:t:"":_:ts) = (rs, goT 0 t, map (goT 0) ts)
  goR rs (l:ls) = goR (goR' [] l:rs) ls where
    goR' :: String -> String -> Rule
    goR' name (':':' ':cs) =
      let (n1, cs')   = goR'' 0 cs
          (n2, cs'')  = goR'' 0 cs'
          (n3, cs''') = goR'' 0 cs''
          (n4, _)     = goR'' 0 cs'''
      in Rule (reverse name) (n1, n2) (n3, n4)
    goR' name (c:cs) = goR' (c:name) cs
    goR' _ _ = error "invalid input"

    goR'' :: Int -> String -> (Int, String)
    goR'' !acc ('-':ds) = (acc, ds)
    goR'' !acc (' ':_:_:_:ds) = (acc, ds)
    goR'' !acc [] = (acc, [])
    goR'' !acc (d:ds) = goR'' (stepParseInt acc d) ds
  goR _ _ = error "invalid input"

  goT :: Int -> String -> [Int]
  goT !acc [] = [acc]
  goT !acc (',':ds) = acc : goT 0 ds
  goT !acc (d:ds) = goT (stepParseInt acc d) ds

checkRule :: Int -> Rule -> Bool
checkRule n rule = checkRange n (rRange1 rule) || checkRange n (rRange2 rule)

checkRange :: Int -> (Int, Int) -> Bool
checkRange n (lo, hi) = n >= lo && n <= hi

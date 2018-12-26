{-# LANGUAGE BangPatterns #-}

module Common where

import Utils

parse :: String -> [((Int, Int, Int), Int)]
{-# INLINABLE parse #-}
parse = map go . lines where
  go ('p':'o':'s':'=':'<':rest) =
    let (x, rest') = goI rest
        (y, rest'') = goI rest'
        (z, ',':' ':'r':'=':rest''') = goI rest''
        (r, _) = goI rest'''
    in ((x, y, z), r)
  go _ = error "invalid input"

  goI ('-':rest) = let (i, r) = goI' 0 rest in (negate i, r)
  goI rest = goI' 0 rest

  goI' !acc (',':rest) = (acc, rest)
  goI' !acc ('>':rest) = (acc, rest)
  goI' !acc (d:rest)   = goI' (stepParseInt acc d) rest
  goI' !acc [] = (acc, [])

{-# LANGUAGE BangPatterns #-}

module Common where

import           Control.Arrow (first)

import           Utils

parse :: String -> [((Int, Int), (Int, Int))]
{-# INLINABLE parse #-}
parse = map go . lines where
  go ('p':'o':'s':'i':'t':'i':'o':'n':'=':rest) =
    let (xy, 'v':'e':'l':'o':'c':'i':'t':'y':'=':rest') = goP rest
        (dxy, _) = goP rest'
    in (xy, dxy)
  go _ = error "invalid input"

  goP ('<':rest) =
    let (x, rest') = goN rest
        (y, rest'') = goN rest'
    in ((x, y), rest'')
  goP _ = error "invalid input"

  goN (' ':rest) = goN' 0 rest
  goN ('-':rest) = first negate (goN' 0 rest)
  goN _ = error "invalid input"

  goN' !acc (',':' ':rest) = (acc, rest)
  goN' !acc ('>':' ':rest) = (acc, rest)
  goN' !acc ('>':rest) = (acc, rest)
  goN' !acc (d:rest) = goN' (stepParseInt acc d) rest
  goN' _ _ = error "invalid input"

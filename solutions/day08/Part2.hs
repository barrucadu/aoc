{-# LANGUAGE BangPatterns #-}

import Control.Arrow (first)
import Data.List (foldl')

import Utils

main :: IO ()
main = mainFor 8 words (show . solve)

solve :: [String] -> Int
solve = fst . goN where
  goN (nchildren:nmetadata:rest) =
    let nchildren' = parseInt nchildren
        nmetadata' = parseInt nmetadata
        (values, rest') = goC nchildren' rest
        (metadatas, rest'') = goM nmetadata' rest'
    in (calculate nchildren' values metadatas, rest'')
  goN _ = error "invalid input"

  goC = goC' [] where
    goC' values  0 rest = (reverse values, rest)
    goC' values !n rest =
      let (value, rest') = goN rest
      in goC' (value:values) (n-1) rest'

  goM n = first (map parseInt) . splitAt n

  calculate 0 _ metadatas = sum metadatas
  calculate nvalues values metadatas = foldl' go 0 metadatas where
    go !acc idx
      | idx > nvalues = acc
      | otherwise = acc + values !! (idx - 1)

{-# LANGUAGE BangPatterns #-}

import Utils

main :: IO ()
main = mainFor 8 words (show . solve)

solve :: [String] -> Int
solve = fst . goN where
  goN (nchildren:nmetadata:rest) =
    let (metadata1, rest') = goC (parseInt nchildren) rest
        (metadata2, rest'') = goM (parseInt nmetadata) rest'
    in (metadata1 + metadata2, rest'')
  goN _ = error "invalid input"

  goC = goC' 0 where
    goC' !metadata !0 rest = (metadata, rest)
    goC' !metadata !n rest =
      let (metadata', rest') = goN rest
      in goC' (metadata + metadata') (n-1) rest'

  goM = goM' 0 where
    goM' !metadata !0 rest = (metadata, rest)
    goM' !metadata !n (m:rest) =
      goM' (metadata + parseInt m) (n-1) rest
    goM' _ _ _ = error "invalid input"

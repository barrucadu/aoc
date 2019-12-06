module Common where

-- needs to be a lazy map because values will be defined self-referentially
import qualified Data.Map.Lazy as M

parse :: String -> M.Map String (String, Int)
{-# INLINE parse #-}
parse input = final where
  final = foldr go m0 (lines input)

  go [a,b,c,')',d,e,f] m =
    let orbitee = [a,b,c]
        orbiter = [d,e,f]
    in M.insert orbiter (orbitee, 1 + getOrbits orbitee) m
  go xs _ = error ("bad input: " ++ xs)

  getOrbits k = snd $ M.findWithDefault (error ("missing orbit count for: " ++ k)) k final

  m0 = M.singleton "COM" ("COM", 0)

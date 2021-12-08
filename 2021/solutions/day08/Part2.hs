import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Set as S

import Utils

main :: IO ()
main = mainFor 8 parse (show . solve)

type Symbol = S.Set Char

parse :: String -> [([Symbol], [Symbol])]
parse = map go . lines where
  go l =
    let (examples, rest) = parseSignals [] [] l
        (outputs, _) = parseSignals [] [] rest
    in (examples, outputs)

  parseSignals :: [Symbol] -> String -> String -> ([Symbol], String)
  parseSignals ss s [] = (S.fromList s:ss, [])
  parseSignals ss _ ('|':' ':rest) = (ss, rest)
  parseSignals ss s (' ':rest) = parseSignals (S.fromList s:ss) [] rest
  parseSignals ss s (c:rest) = parseSignals ss (c:s) rest

solve :: [([Symbol], [Symbol])] -> Int
solve = sum . map (uncurry go) where
  go :: [Symbol] -> [Symbol] -> Int
  go examples outputs =
    let mapping = decode examples
    in foldl' (\acc d -> 10 * acc + fromJust (lookup d mapping)) 0 (reverse outputs)

  decode :: [Symbol] -> [(Symbol, Int)]
  decode examples =
    let withSegments n = filter ((==n) . S.size) examples
        hasSubsymbol small = filter (small `S.isSubsetOf`)
        isSubsymbolOf big = filter (`S.isSubsetOf` big)
        isNot a = filter (/=a)
        one = head $ withSegments 2
        four = head $ withSegments 4
        seven = head $ withSegments 3
        eight = head $ withSegments 7
        three = head . hasSubsymbol seven $ withSegments 5
        nine = head . hasSubsymbol four $ withSegments 6
        zero = head . filter (\e -> S.size (one `S.intersection` e) == 2) . isNot nine $ withSegments 6
        six = head . isNot zero . isNot nine $ withSegments 6
        five = head . isSubsymbolOf nine . isNot three $ withSegments 5
        two = head . isNot five . isNot three $ withSegments 5
    in [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
        

import Data.List (foldl')
import qualified Data.Map as M

import Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: BagRules -> Int
solve rules = go1 target where
  go1 = go2 . M.toList . grab

  go2 ((col, count):cs) = count * go1 col + count + go2 cs
  go2 [] = 0

  grab k = M.findWithDefault M.empty k rules

target :: Colour
target = ("shiny", "gold")

-------------------------------------------------------------------------------

type Colour = (String, String)

type BagRules = M.Map Colour (M.Map Colour Int)

parse :: String -> BagRules
parse = foldl' parseLine M.empty . map words . lines where
  parseLine :: BagRules -> [String] -> BagRules
  parseLine acc (c1:c2:_:_:contains) = parseContains (c1, c2) acc contains
  parseLine _ _ = error "invalid input"

  parseContains :: Colour -> BagRules -> [String] -> BagRules
  parseContains col = parseContains' where
    parseContains' :: BagRules -> [String] -> BagRules
    parseContains' acc (n:c1:c2:_:contains) =
      let acc' = M.insertWith M.union col (M.singleton (c1, c2) (parseInt n)) acc
      in parseContains' acc' contains
    parseContains' acc [] = acc
    parseContains' acc [_, _, _] = acc
    parseContains' _ _ = error "invalid input"

import           Data.List (foldl')
import qualified Data.Map  as M
import qualified Data.Set  as S

import           Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

solve :: InvertedBagRules -> Int
solve rules = go seen0 target0 where
  go :: S.Set Colour -> S.Set Colour -> Int
  go seen candidates
    | S.null candidates = S.size seen - 1
    | otherwise =
      let col = S.elemAt 0 candidates
          seen' = S.insert col seen
          candidates' = S.delete col candidates `S.union` (grab col `S.difference` seen)
      in go seen' candidates'

  seen0 = S.singleton target
  target0 = grab target

  grab k = M.findWithDefault S.empty k rules

target :: Colour
target = ("shiny", "gold")

-------------------------------------------------------------------------------

type Colour = (String, String)

type InvertedBagRules = M.Map Colour (S.Set Colour)

parse :: String -> InvertedBagRules
parse = foldl' parseLine M.empty . map words . lines where
  parseLine :: InvertedBagRules -> [String] -> InvertedBagRules
  parseLine acc (c1:c2:_:_:contains) = parseContains (c1, c2) acc contains
  parseLine _ _ = error "invalid input"

  parseContains :: Colour -> InvertedBagRules -> [String] -> InvertedBagRules
  parseContains col = parseContains' where
    parseContains' :: InvertedBagRules -> [String] -> InvertedBagRules
    parseContains' acc (_:c1:c2:_:contains) =
      let acc' = M.insertWith S.union (c1, c2) (S.singleton col) acc
      in parseContains' acc' contains
    parseContains' acc [] = acc
    parseContains' acc [_, _, _] = acc
    parseContains' _ _ = error "invalid input"

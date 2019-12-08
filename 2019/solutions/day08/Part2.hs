import           GHC.Exts (build)
import           Utils

main :: IO ()
main = mainFor 8 parse solve

parse :: String -> [String]
parse = chunksOf layerSize where
  layerSize = width * height

solve :: [String] -> String
solve (l0:ls0) = unlines . chunksOf width . map display $ go l0 ls0 where
  go l [] = l
  go l ["\n"] = l
  go l (l1:ls) = go (zipWith combine l l1) ls

  combine '2' x = x
  combine x _   = x

  display '0' = ' '
  display _   = '#'
solve _ = error "bad input"

width, height :: Int
width  = 25
height = 6

-- modified from split-0.2.3.3
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n


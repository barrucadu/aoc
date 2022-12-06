import           Utils

main :: IO ()
main = mainFor 6 solve show

solve :: String -> Int
solve = go 0 where
  go pos (a:rest@(b:c:d:_))
    | isMarker a b c d = pos + 4
    | otherwise = go (pos+1) rest
  go _ _ = error "no solution!"

  isMarker a b c d =
    a /= b && a /= c && a /= d &&
    b /= c && b /= d &&
    c /= d

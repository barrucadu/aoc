module Common where

parse :: String -> [Int]
parse = map go . lines where
  go [r1, r2, r3, r4, r5, r6, r7, c1, c2, c3] =
    let row = goR r1 r2 r3 r4 r5 r6 r7
        col = goC c1 c2 c3
    in row * 8 + col
  go _ = error "invalid input"

  goR r1 r2 r3 r4 r5 r6 r7 =
    check 'B' 64 r1 +
    check 'B' 32 r2 +
    check 'B' 16 r3 +
    check 'B' 8 r4 +
    check 'B' 4 r5 +
    check 'B' 2 r6 +
    check 'B' 1 r7

  goC c1 c2 c3 =
    check 'R' 4 c1 +
    check 'R' 2 c2 +
    check 'R' 1 c3

  check c1 v c2 = if c1 == c2 then v else 0

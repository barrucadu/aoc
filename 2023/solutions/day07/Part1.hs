import Data.List (sort)

import Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

data HType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Eq, Ord, Show)

data Hand = Hand HType String
  deriving (Eq, Ord, Show)

parse :: String -> [(Hand, Int)]
parse = map (go . words) . lines where
  go [cards, bid] = (toHand cards, parseInt bid)
  toHand cards = Hand (typeOf cards) (map ordHack cards)

  ordHack 'A' = 'Z'
  ordHack 'K' = 'Y'
  ordHack 'Q' = 'X'
  ordHack 'J' = 'W'
  ordHack 'T' = 'V'
  ordHack c = c

solve :: [(Hand, Int)] -> Int
solve hands = sum [ rank * bid | (rank, (_, bid)) <- zip [1..] (sort hands) ]

typeOf :: String -> HType
typeOf s0
    | abcde = FiveKind
    | abcd || bcde = FourKind
    | (abc && de) || (ab && cde) = FullHouse
    | abc || bcd || cde = ThreeKind
    | (ab && cd) || (ab && de) || (bc && de) = TwoPair
    | ab || bc || cd || de = OnePair
    | otherwise = HighCard
  where
    [a,b,c,d,e] = sort s0

    ab = a == b
    bc = b == c
    cd = c == d
    de = d == e

    abc = ab && bc
    bcd = bc && cd
    cde = cd && de

    abcd = abc && cd
    bcde = bcd && de

    abcde = abcd && de

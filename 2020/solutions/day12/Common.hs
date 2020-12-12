module Common where

import Utils

data Point = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Read, Show)

data Facing = FN | FS | FE | FW
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Instr
  = N {-# UNPACK #-} !Int
  | S {-# UNPACK #-} !Int
  | E {-# UNPACK #-} !Int
  | W {-# UNPACK #-} !Int
  | L {-# UNPACK #-} !Int
  | R {-# UNPACK #-} !Int
  | F {-# UNPACK #-} !Int
  deriving (Eq, Ord, Read, Show)

parse :: String -> [Instr]
parse = map go . lines where
  go ('N':ns) = N (parseInt ns)
  go ('S':ns) = S (parseInt ns)
  go ('E':ns) = E (parseInt ns)
  go ('W':ns) = W (parseInt ns)
  go ('L':ns) = L (parseInt ns)
  go ('R':ns) = R (parseInt ns)
  go ('F':ns) = F (parseInt ns)
  go _ = error "invalid input"

shift :: Facing -> Int -> Point -> Point
shift FN d (P x y) = P x (y-d)
shift FS d (P x y) = P x (y+d)
shift FE d (P x y) = P (x+d) y
shift FW d (P x y) = P (x-d) y

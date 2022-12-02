module Common where

data RPS = Rock | Paper | Scissors
  deriving Eq

beats :: RPS -> RPS -> Bool
rps1 `beats` rps2 = rps1 == victor rps2

victor :: RPS -> RPS
victor Rock = Paper
victor Paper = Scissors
victor Scissors = Rock

loser :: RPS -> RPS
loser Rock = Scissors
loser Paper = Rock
loser Scissors = Paper

throwScore :: RPS -> Int
throwScore Rock = 1
throwScore Paper = 2
throwScore Scissors = 3

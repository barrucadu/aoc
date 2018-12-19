{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.ST (ST, runST)

import Common
import Utils

main :: IO ()
main = mainFor 18 id (show . solve)

solve :: String -> Int
solve input = runST $ do
    let dim@(w, h) = getWH input
    s0 <- newArray w h
    parse s0 input
    (mu, lambda, scores) <- floyd dim s0
    let pos = (steps - mu) `mod` lambda - 1
    pure (scores !! pos)
  where
    steps = 1000000000

    step' :: Dimensions -> Automata s -> ST s (Automata s)
    step' dim@(w, h) s = do
      s' <- newArray w h
      step dim s s'
      pure s'

    floyd :: forall s. Dimensions -> Automata s -> ST s (Int, Int, [Int])
    floyd dim@(w, h) s0 = do
        -- Find a repetition x_i = x_2i
        tortoise <- step' dim s0
        hare     <- step' dim =<< step' dim s0
        hare'    <- findRepetition tortoise hare

        -- Find the position μ of first repetition.
        (mu, tortoise') <- findMu s0 hare'

        -- Find the scores starting from x_μ (list of scores is more
        -- helpful in this problem than just the cycle length)
        (lambda, scores) <- findScores tortoise'

        pure (mu, lambda, scores)
      where
        findRepetition tortoise hare = check tortoise hare >>= \case
          True  -> pure hare
          False -> do
            tortoise' <- step' dim tortoise
            hare'     <- step' dim =<< step' dim hare
            findRepetition tortoise' hare'

        findMu = go 0 where
          go !acc tortoise hare = check tortoise hare >>= \case
            True  -> pure (acc, tortoise)
            False -> do
              tortoise' <- step' dim tortoise
              hare'     <- step' dim hare
              go (acc+1) tortoise' hare'

        findScores tortoise = do
            hare0  <- step' dim tortoise
            score0 <- score dim hare0
            go [score0] hare0
          where
            go scores hare = check tortoise hare >>= \case
              True  -> pure (length scores, reverse scores)
              False -> do
                hare'  <- step' dim hare
                score' <- score dim hare'
                go (score':scores) hare'

        check :: Automata s -> Automata s -> ST s Bool
        check s1 s2 = go 0 0 where
          go x y
            | y == h = pure True
            | x == w = go 0 (y+1)
            | otherwise = do
                c1 <- readArray s1 x y
                c2 <- readArray s2 x y
                if c1 == c2
                  then go (x+1) y
                  else pure False

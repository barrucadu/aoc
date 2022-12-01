import           Data.List (isPrefixOf, transpose)

import           Data.Set  ((\\))
import qualified Data.Set  as S

import           Common
import           Utils

main :: IO ()
main = mainFor 16 parse (show . solve)

solve :: ([Rule], Ticket, [Ticket]) -> Int
solve (rules, myTicket, tickets) = product [ n | (field, n) <- zip fieldNames myTicket, "departure" `isPrefixOf` field ] where
  fieldNames = propagateR True [] (map intersections $ transpose validTickets) where
    intersections = foldl1 S.intersection

    propagateR allSingletons lefts [current] = case (allSingletons, isSingleton current) of
      (True, True)   -> reverse $ map getElement (current:lefts)
      (True, False)  -> error "no solution"
      (False, True)  -> propagateL True (rmElement current lefts) [current]
      (False, False) -> propagateL False lefts [current]
    propagateR allSingletons lefts (current:rights) = case (allSingletons, isSingleton current) of
      (True, True)   -> propagateR True  (current:lefts) (rmElement current rights)
      (True, False)  -> propagateR False (current:lefts) rights
      (False, True)  -> propagateR False (current:rmElement current lefts) (rmElement current rights)
      (False, False) -> propagateR False (current:lefts) rights
    propagateR _ _ _ = error "invalid input"

    propagateL allSingletons [current] rights = case (allSingletons, isSingleton current) of
      (True, True)   -> map getElement (current:rights)
      (True, False)  -> error "no solution"
      (False, True)  -> propagateR True [current] (rmElement current rights)
      (False, False) -> propagateR False [current] rights
    propagateL allSingletons (current:lefts) rights = case (allSingletons, isSingleton current) of
      (True, True)   -> propagateL True  (rmElement current lefts) (current:rights)
      (True, False)  -> propagateL False  lefts (current:rights)
      (False, True)  -> propagateL False (rmElement current lefts) (current:rmElement current rights)
      (False, False) -> propagateL False lefts (current:rights)
    propagateL _ _ _ = error "invalid input"

    getElement = S.elemAt 0

    rmElement x = map (\\x)

    isSingleton s = S.size s == 1

  validTickets = filter valid $ map matchRulesToFields tickets where
    matchRulesToFields = map mRTF

    mRTF field = S.fromList [ rName rule | rule <- rules, checkRule field rule ]

    valid = all (not . S.null)

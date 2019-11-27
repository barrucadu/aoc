{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Common where

import           Control.Monad.ST    (ST, runST)
import           Data.Foldable       (for_)
import           Data.List           (sort, sortOn)
import           Data.Ord            (Down(..))
import qualified Data.Vector.Mutable as V

import           Utils

data AttackType
  = Fire
  | Radiation
  | Bludgeoning
  | Slashing
  | Cold
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Army = Army
  { units      :: !Int
  , unitHP     :: !Int
  , unitDamage :: !Int
  , initiative :: !Int
  , weaknesses :: [AttackType]
  , immunities :: [AttackType]
  , attackType :: AttackType
  }
  deriving (Eq, Ord, Read, Show)

data Winner = ImmuneSystem | Infection
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

parse :: String -> ([Army], [Army])
{-# INLINABLE parse #-}
parse input0 =
    let ("Immune System:":rest) = lines input0
        (immuneSystem, "Infection:":rest') = parseArmy [] rest
        (infection, _) = parseArmy [] rest'
    in (immuneSystem, infection)
  where
    parseArmy army ([]:rest) = (army, rest)
    parseArmy army (a:rest) = parseArmy (parseArmy' (words a) : army) rest
    parseArmy army [] = (army, [])

    parseArmy' (sunits:"units":"each":"with":sunithp:"hit":"points":rest) =
      let (ws, is, ["with", "an", "attack", "that", "does", sunitdamage, sattacktype, "damage", "at", "initiative", sinitiative]) = parseWI rest
      in Army
         { units      = parseInt sunits
         , unitHP     = parseInt sunithp
         , unitDamage = parseInt sunitdamage
         , initiative = parseInt sinitiative
         , weaknesses = ws
         , immunities = is
         , attackType = fst (parseAT sattacktype)
         }

    parseWI ("(weak":"to":rest) = case parseWI' [] rest of
      (ws, "immune":"to":rest') ->
        let (is, rest'') = parseWI' [] rest'
        in (ws, is, rest'')
      (ws, rest') -> (ws, [], rest')
    parseWI ("(immune":"to":rest) = case parseWI' [] rest of
      (is, "weak":"to":rest') ->
        let (ws, rest'') = parseWI' [] rest'
        in (ws, is, rest'')
      (is, rest') -> ([], is, rest')
    parseWI rest = ([], [], rest)

    parseWI' ats (a:as) = case parseAT a of
      (at, ",") -> parseWI' (at:ats) as
      (at, []) -> parseWI' (at:ats) as
      (at, _)  -> (at:ats, as)

    parseAT ('f':'i':'r':'e':rest) = (Fire, rest)
    parseAT ('r':'a':'d':'i':'a':'t':'i':'o':'n':rest) = (Radiation, rest)
    parseAT ('b':'l':'u':'d':'g':'e':'o':'n':'i':'n':'g':rest) = (Bludgeoning, rest)
    parseAT ('s':'l':'a':'s':'h':'i':'n':'g':rest) = (Slashing, rest)
    parseAT ('c':'o':'l':'d':rest) = (Cold, rest)
    parseAT at = error ("unknown at: " ++ at)

fight :: ([Army], [Army]) -> Maybe (Int, Winner)
{-# INLINABLE fight #-}
fight (immuneSystem0, infection0) = runST $ do
    immuneSystem <- toArmies immuneSystem0
    infection    <- toArmies infection0
    battle immuneSystem infection

-- invariant: armies vectors are sorted in attack order (initiative descending)
toArmies :: [Army] -> ST s (V.STVector s (Army, Maybe Int))
{-# INLINABLE toArmies #-}
toArmies as = do
  let sorted = sortOn (Down . initiative) as
  let len = length as
  v <- V.unsafeNew len
  for_ (zip [0..] sorted) $ \(i, a) -> V.unsafeWrite v i (a, Nothing)
  pure v

-- battle two groups against each other, mutating the original vectors
battle :: V.STVector s (Army, Maybe Int) -> V.STVector s (Army, Maybe Int) -> ST s (Maybe (Int, Winner))
{-# INLINABLE battle #-}
battle group1 group2 = loop (-1) (-1) where
  loop 0 score2 = pure (Just (score2, Infection))
  loop score1 0 = pure (Just (score1, ImmuneSystem))
  loop score1 score2 = do
    ok <- targetSelection =<< epowerOrder
    attack
    score1' <- score group1
    score2' <- score group2
    if score1 /= score1' || score2 /= score2'
      then loop score1' score2'
      else pure Nothing

  epowerOrder = do
      o1 <- go 1 group1 0
      o2 <- go 2 group2 0
      pure (sortOn Down (o1 ++ o2))
    where
      go g v !i
        | i == V.length v = pure []
        | otherwise = do
            (a, _) <- V.unsafeRead v i
            ((effectivePower a, initiative a, g, i):) <$> go g v (i+1)

  targetSelection order0 = do
      assigned1 <- V.unsafeNew (V.length group1)
      assigned2 <- V.unsafeNew (V.length group2)
      V.set assigned1 False
      V.set assigned2 False
      select assigned1 assigned2
    where
      select assigned1 assigned2 = go order0 where
        go ((_,_,1,i):os) = do
          (army, _) <- V.unsafeRead group1 i
          choice <- choose army group2 assigned2
          case choice of
            Just chosen -> do
              V.unsafeWrite group1 i (army, choice)
              V.unsafeWrite assigned2 chosen True
              go os
            Nothing -> go os
        go ((_,_,2,i):os) = do
          (army, _) <- V.unsafeRead group2 i
          choice <- choose army group1 assigned1
          case choice of
            Just chosen -> do
              V.unsafeWrite group2 i (army, choice)
              V.unsafeWrite assigned1 chosen True
              go os
            Nothing -> go os
        go _ = pure ()

      choose attacker defenders assigned = go Nothing 0 where
        go best !i
          | i == V.length defenders = pure (fst <$> best)
          | otherwise = do
              (defender, _) <- V.unsafeRead defenders i
              isAssigned <- V.unsafeRead assigned i
              if | units defender <= 0 -> go best (i+1)
                 | isAssigned -> go best (i+1)
                 | isBetterThan attacker defender best -> go (Just (i, defender)) (i+1)
                 | otherwise -> go best (i+1)

        isBetterThan attacker defender1 (Just (_, defender2))
          | damage attacker defender1 > damage attacker defender2 = True
          | damage attacker defender1 < damage attacker defender2 = False
          | effectivePower defender1 > effectivePower defender2 = True
          | effectivePower defender1 < effectivePower defender2 = False
          | initiative defender1 > initiative defender2 = True
          | otherwise = False
        isBetterThan attacker defender Nothing = damage attacker defender > 0

  attack = go 0 0 where
    go i1 i2
      | i1 == V.length group1 && i2 == V.length group2 = pure ()
      | i1 == V.length group1 = doAttack group2 group1 i2 >> go i1 (i2+1)
      | i2 == V.length group2 = doAttack group1 group2 i1 >> go (i1+1) i2
      | otherwise = do
          (candidate1, _) <- V.unsafeRead group1 i1
          (candidate2, _) <- V.unsafeRead group2 i2
          if initiative candidate1 > initiative candidate2
            then doAttack group1 group2 i1 >> go (i1+1) i2
            else doAttack group2 group1 i2 >> go i1 (i2+1)

    doAttack g1 g2 i = V.unsafeRead g1 i >>= \case
      (attacker, Just target) | units attacker > 0 -> do
        (defender, defenderTarget) <- V.unsafeRead g2 target
        let killed = damage attacker defender `div` unitHP defender
        let defender' = defender { units = max 0 (units defender - killed) }
        V.unsafeWrite g2 target (defender', if units defender' <= 0 then Nothing else defenderTarget)
        V.unsafeWrite g1 i (attacker, Nothing)
      _ -> pure ()

score :: V.STVector s (Army, a) -> ST s Int
{-# INLINABLE score #-}
score armies = go 0 0 where
  go !acc !i
    | i >= V.length armies = pure acc
    | otherwise = do
        (army, _) <- V.unsafeRead armies i
        go (acc + units army) (i+1)

effectivePower :: Army -> Int
{-# INLINABLE effectivePower #-}
effectivePower a = units a * unitDamage a

damage :: Army -> Army -> Int
{-# INLINABLE damage #-}
damage attacker defender
  | attackType attacker `elem` immunities defender = 0
  | attackType attacker `elem` weaknesses defender = 2 * effectivePower attacker
  | otherwise = effectivePower attacker

-- handy for debugging
ppV :: Show a => V.STVector s (Army, a) -> ST s String
ppV v = go [] 0 where
  go acc n
    | n == V.length v = do
        s <- score v
        pure (unlines (reverse (("score: " ++ show s):acc)))
    | otherwise = do
        (a, x) <- V.unsafeRead v n
        go (show (n, effectivePower a, a, x) : acc) (n+1)

{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.Map as M

import           Utils

type Ingredient = String

data Recipe = Recipe
  { rOutput :: {-# UNPACK #-} !Int
  , rIngredients :: [(Int, Ingredient)]
  } deriving Show

parse :: String -> M.Map Ingredient Recipe
{-# INLINABLE parse #-}
parse = go M.empty . lines where
  go map_ [] = map_
  go map_ (l:ls) =
    let (ingredients, rest) = parseIngredients l
        (quantity, ingredient, []) = parseIngredient rest
    in go (M.insert ingredient (Recipe quantity ingredients) map_) ls

  parseIngredients = goP [] where
    goP is rest = case parseIngredient rest of
      (q, n, ' ':'=':'>':' ':rest') -> ((q, n):is, rest')
      (q, n, ',':' ':rest') -> goP ((q, n):is) rest'
      _ -> error "bad input"

  parseIngredient = goQ 0 where
    goQ !acc (' ':rest) = goI acc [] rest
    goQ !acc (c:rest) = goQ (stepParseInt acc c) rest
    goQ _ _ = error "bad input"

    goI q n rest@(',':' ':_) = (q, reverse n, rest)
    goI q n rest@(' ':'=':'>':' ':_) = (q, reverse n, rest)
    goI q n (c:rest) = goI q (c:n) rest
    goI q n [] = (q, reverse n, [])

makeFuel :: M.Map Ingredient Recipe -> Int -> Int
{-# INLINABLE makeFuel #-}
makeFuel recipes = go . M.singleton "FUEL" . negate where
  go :: M.Map Ingredient Int -> Int
  go ingredients
    | M.foldrWithKey check True ingredients = - M.findWithDefault (error "missing ore value") "ORE" ingredients
    | otherwise = go . M.unionsWith (+) . map (M.fromList . manufacture) . M.toList $ ingredients

  manufacture :: (Ingredient, Int) -> [(Ingredient, Int)]
  manufacture ("ORE", quantity) = [("ORE", quantity)]
  manufacture (name, quantity)
    | quantity >= 0 = [(name, quantity)]
    | otherwise =
      let Recipe q is = recipe name
          -- + q - 1 because we've got to make at least q units, but
          -- if we already needed q we don't want to make 2q units
          --
          -- an alternative would be using divMod and adding 1 to the
          -- factor if the mod is nonzero
          factor = (negate quantity + q - 1) `div` q
      in (name, quantity + factor * q) : [(name', negate factor * quantity') | (quantity', name') <- is]

  recipe n = M.findWithDefault (error ("missing recipe for: " ++ n)) n recipes

  check "ORE" _ b = b
  check _ q b = b && q >= 0

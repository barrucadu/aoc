{-# LANGUAGE BangPatterns #-}

module Common where

import qualified Data.Map.Strict as M

-- (Y, X) so that iterating over a map goes in the right order for the
-- carts.
type Pos = (Int, Int)

data Event      = EBckSlash | EFwdSlash | ERotate
data SDirection = SUp | SDown | SLeft | SRight
data TDirection = TLeft | TFwd | TRight

data Cart = Cart
  { stepDirection :: !SDirection
  , turnDirection :: !TDirection
  }

parse :: String -> (M.Map Pos Event, M.Map Pos Cart)
{-# INLINABLE parse #-}
parse = go 0 0 M.empty M.empty where
  go  _  _ em pm [] = (em, pm)
  go !y  _ em pm ('\n':es) = go (y+1) 0 em pm es
  go !y !x em pm ('\\':es) = go y (x+1) (M.insert (y, x) EBckSlash em) pm es
  go !y !x em pm ('/':es)  = go y (x+1) (M.insert (y, x) EFwdSlash em) pm es
  go !y !x em pm ('+':es)  = go y (x+1) (M.insert (y, x) ERotate   em) pm es
  go !y !x em pm ('>':es)  = go y (x+1) em (M.insert (y, x) (Cart SRight TLeft) pm) es
  go !y !x em pm ('<':es)  = go y (x+1) em (M.insert (y, x) (Cart SLeft  TLeft) pm) es
  go !y !x em pm ('^':es)  = go y (x+1) em (M.insert (y, x) (Cart SUp    TLeft) pm) es
  go !y !x em pm ('v':es)  = go y (x+1) em (M.insert (y, x) (Cart SDown  TLeft) pm) es
  go !y !x em pm (_:es)    = go y (x+1) em pm es

stepCart :: M.Map Pos Event -> (Pos, Cart) -> (Pos, Cart)
{-# INLINABLE stepCart #-}
stepCart em ((y, x), cart) = (yx', cart') where
  yx' = case stepDirection cart of
    SUp    -> (y-1, x)
    SDown  -> (y+1, x)
    SLeft  -> (y, x-1)
    SRight -> (y, x+1)

  cart' = case M.lookup yx' em of
    Just EBckSlash -> case stepDirection cart of
      SUp    -> cart { stepDirection = SLeft  }
      SDown  -> cart { stepDirection = SRight }
      SLeft  -> cart { stepDirection = SUp    }
      SRight -> cart { stepDirection = SDown  }
    Just EFwdSlash -> case stepDirection cart of
      SUp    -> cart { stepDirection = SRight }
      SDown  -> cart { stepDirection = SLeft  }
      SLeft  -> cart { stepDirection = SDown  }
      SRight -> cart { stepDirection = SUp    }
    Just ERotate -> case turnDirection cart of
      TLeft  -> case stepDirection cart of
        SUp    -> cart { stepDirection = SLeft,  turnDirection = TFwd }
        SDown  -> cart { stepDirection = SRight, turnDirection = TFwd }
        SLeft  -> cart { stepDirection = SDown,  turnDirection = TFwd }
        SRight -> cart { stepDirection = SUp,    turnDirection = TFwd }
      TFwd   -> cart { turnDirection = TRight }
      TRight -> case stepDirection cart of
        SUp    -> cart { stepDirection = SRight, turnDirection = TLeft }
        SDown  -> cart { stepDirection = SLeft,  turnDirection = TLeft }
        SLeft  -> cart { stepDirection = SUp,    turnDirection = TLeft }
        SRight -> cart { stepDirection = SDown,  turnDirection = TLeft }
    Nothing -> cart

checkCollision :: M.Map Pos Cart -> Pos -> [(Pos, Cart)] -> Bool
{-# INLINABLE checkCollision #-}
checkCollision pm yx0 cs0 = yx0 `M.member` pm || go cs0 where
  go ((yx,_):cs)
    | yx == yx0 = True
    | yx > yx0  = False
    | otherwise = go cs
  go [] = False

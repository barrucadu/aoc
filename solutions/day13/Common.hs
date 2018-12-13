{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word (Word8)

-- (Y, X) so that iterating over a map goes in the right order for the
-- carts.
type Pos = (Int, Int)

type Event = Word8
pattern EBckSlash <- 1 where EBckSlash = 1
pattern EFwdSlash <- 2 where EFwdSlash = 2
pattern ERotate   <- 3 where ERotate   = 3

data SDirection = SUp | SDown | SLeft | SRight
data TDirection = TLeft | TFwd | TRight

data Cart = Cart
  { stepDirection :: !SDirection
  , turnDirection :: !TDirection
  }

type Events = (Int, V.Vector Event)

parse :: String -> (Events, M.Map Pos Cart)
{-# INLINABLE parse #-}
parse = go 0 0 [] M.empty where
  go :: Int -> Int -> [((Int, Int), Event)] -> M.Map Pos Cart -> String -> (Events, M.Map Pos Cart)
  go !y !x em pm "\n" = ((x, fromEM y x em), pm)
  go !y  _ em pm ('\n':es) = go (y+1) 0 em pm es
  go !y !x em pm ('\\':es) = go y (x+1) (((y, x), EBckSlash):em) pm es
  go !y !x em pm ('/':es)  = go y (x+1) (((y, x), EFwdSlash):em) pm es
  go !y !x em pm ('+':es)  = go y (x+1) (((y, x), ERotate):em)   pm es
  go !y !x em pm ('>':es)  = go y (x+1) em (M.insert (y, x) (Cart SRight TLeft) pm) es
  go !y !x em pm ('<':es)  = go y (x+1) em (M.insert (y, x) (Cart SLeft  TLeft) pm) es
  go !y !x em pm ('^':es)  = go y (x+1) em (M.insert (y, x) (Cart SUp    TLeft) pm) es
  go !y !x em pm ('v':es)  = go y (x+1) em (M.insert (y, x) (Cart SDown  TLeft) pm) es
  go !y !x em pm (_:es)    = go y (x+1) em pm es
  go _ _ _ _ _ = error "invalid input"

  fromEM height width em = V.create $ do
    v <- VM.new (height * width)
    for_ em $ \((y, x), e) -> VM.unsafeWrite v (y * width + x) e
    pure v

stepCart :: Events -> (Pos, Cart) -> (Pos, Cart)
{-# INLINABLE stepCart #-}
stepCart (width, ev) ((y, x), cart) = (yx', cart') where
  yx'@(y', x') = case stepDirection cart of
    SUp    -> (y-1, x)
    SDown  -> (y+1, x)
    SLeft  -> (y, x-1)
    SRight -> (y, x+1)

  cart' = case V.unsafeIndex ev (y' * width + x') of
    EBckSlash -> case stepDirection cart of
      SUp    -> cart { stepDirection = SLeft  }
      SDown  -> cart { stepDirection = SRight }
      SLeft  -> cart { stepDirection = SUp    }
      SRight -> cart { stepDirection = SDown  }
    EFwdSlash -> case stepDirection cart of
      SUp    -> cart { stepDirection = SRight }
      SDown  -> cart { stepDirection = SLeft  }
      SLeft  -> cart { stepDirection = SDown  }
      SRight -> cart { stepDirection = SUp    }
    ERotate -> case turnDirection cart of
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
    _ -> cart

checkCollision :: M.Map Pos Cart -> Pos -> [(Pos, Cart)] -> Bool
{-# INLINABLE checkCollision #-}
checkCollision pm yx0 cs0 = yx0 `M.member` pm || go cs0 where
  go ((yx,_):cs)
    | yx == yx0 = True
    | yx > yx0  = False
    | otherwise = go cs
  go [] = False

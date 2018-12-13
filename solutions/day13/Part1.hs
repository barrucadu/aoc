{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as M

import Utils

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

main :: IO ()
main = mainFor 13 parse (show . solve)

parse :: String -> (M.Map Pos Event, M.Map Pos Cart)
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

solve :: (M.Map Pos Event, M.Map Pos Cart) -> (Int, Int)
solve (em, pm0) = go pm0 where
  go pm = case step pm of
    Right pm' -> go pm'
    Left (y', x') -> (x', y')

  step = step' M.empty . M.assocs where
    step' pm (c:cs) =
      let (yx, cart) = stepCart c
      in if yx `M.member` pm
         then Left yx
         else step' (M.insert yx cart pm) cs
    step' pm [] = Right pm

  stepCart ((y, x), cart) =
    let yx' = case stepDirection cart of
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
    in (yx', cart')

import           Data.Char  (isDigit)
import           Data.List  (tails)
import           Data.Maybe (mapMaybe)

import           Utils

main :: IO ()
main = mainFor 1 solve show

solve :: String -> Int
solve = sum . map go . lines where
  go cs =
    let digits = mapMaybe fromDigit (tails cs)
    in head digits * 10 + last digits

fromDigit :: String -> Maybe Int
fromDigit ('o':'n':'e':_) = Just 1
fromDigit ('t':'w':'o':_) = Just 2
fromDigit ('t':'h':'r':'e':'e':_) = Just 3
fromDigit ('f':'o':'u':'r':_) = Just 4
fromDigit ('f':'i':'v':'e':_) = Just 5
fromDigit ('s':'i':'x':_) = Just 6
fromDigit ('s':'e':'v':'e':'n':_) = Just 7
fromDigit ('e':'i':'g':'h':'t':_) = Just 8
fromDigit ('n':'i':'n':'e':_) = Just 9
fromDigit (d:_)
  | isDigit d = Just (parseDigit d)
  | otherwise = Nothing
fromDigit _ = Nothing

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Utils where

import           Control.Monad.ST            (ST, runST)
import           Data.Char                   (chr, ord)
import           Data.List                   (foldl')
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- | Common main function
mainFor :: Int -> (String -> a) -> (a -> String) -> IO ()
{-# INLINE mainFor #-}
mainFor dayN parse solve = do
  let n = if dayN < 10 then '0' : show dayN else show dayN
  input <- parse <$> readFile ("../inputs/day" ++ n ++ ".txt")
  putStrLn (solve input)

-- | Manhattan distance between two points in 2D space
manhattan2 :: (Int, Int) -> (Int, Int) -> Int
{-# INLINE manhattan2 #-}
manhattan2 (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Manhattan distance between two points in 3D space
manhattan3 :: (Int, Int, Int) -> (Int, Int, Int) -> Int
{-# INLINE manhattan3 #-}
manhattan3 (x1,y1,z1) (x2,y2,z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

-- | Manhattan distance between two points in 4D space
manhattan4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int
{-# INLINE manhattan4 #-}
manhattan4 (x1,y1,z1,t1) (x2,y2,z2,t2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (t1 - t2)

-- | Min/max from a nonempty list, in pass.
minmax :: Ord a => [a] -> (a, a)
minmax (x0:xs) = foldr (\x (!mi, !ma) -> (min x mi, max x ma)) (x0, x0) xs
minmax _ = error "empty list"

-- | Median of a sorted nonempty list.
median :: [a] -> a
{-# INLINE median #-}
median xs = xs !! pos where
  pos = length xs `div` 2

-- | Lowercase a char.  Assumes it's an ASCII letter.
lowercase :: Char -> Char
{-# INLINE lowercase #-}
lowercase c =
  let diff = ord 'a' - ord 'A'
      o = ord c
  in if o > ord 'Z'
     then c
     else chr (o + diff)

-- | Converts a number to an integer.  Assumes no leading "+" or "-".
parseInt :: String -> Int
{-# INLINE parseInt #-}
parseInt = foldl' stepParseInt 0

{- | Converts a char to a digit.

Using 'ord' here rather than 'digitToInt' is slightly faster for day 1
part 1.

With 'digitToInt':

@
    benchmarking ./Day1Part1
    time                 2.759 ms   (2.749 ms .. 2.770 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 2.755 ms   (2.748 ms .. 2.762 ms)
    std dev              24.23 μs   (19.88 μs .. 32.03 μs)
@

With 'ord':

@
    benchmarking ./Day1Part1
    time                 2.756 ms   (2.746 ms .. 2.767 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 2.743 ms   (2.736 ms .. 2.748 ms)
    std dev              18.28 μs   (13.35 μs .. 27.72 μs)
@
-}
stepParseInt :: Int -> Char -> Int
{-# INLINE stepParseInt #-}
stepParseInt acc c = acc * 10 + parseDigit c

-- | Parse a single digit.
parseDigit :: Char -> Int
{-# INLINE parseDigit #-}
parseDigit c = ord c - ord '0'

-------------------------------------------------------------------------------
-- * Vector-backed Arrays

-- | A pair of the width and the underlying vector.
type STArray s a = (Int, VM.STVector s a)

-- | An immutable 'STArray'
type Array a = (Int, V.Vector a)

newArray :: VM.Unbox a => Int -> Int -> ST s (STArray s a)
{-# INLINE newArray #-}
newArray width height = do
  v <- VM.new (width * height)
  pure (width, v)

writeArray :: VM.Unbox a => STArray s a -> Int -> Int -> a -> ST s ()
{-# INLINE writeArray #-}
writeArray (width, v) x y = VM.unsafeWrite v (x + y * width)

setArray :: VM.Unbox a => STArray s a -> a -> ST s ()
{-# INLINE setArray #-}
setArray (_, v) = VM.set v

readArray :: VM.Unbox a => STArray s a -> Int -> Int -> ST s a
{-# INLINE readArray #-}
readArray (width, v) x y = VM.unsafeRead v (x + y * width)

cloneArray :: VM.Unbox a => STArray s a -> ST s (STArray s a)
{-# INLINE cloneArray #-}
cloneArray (width, v) = do
  v' <- VM.clone v
  pure (width, v')

createArray :: VM.Unbox a => (forall s. ST s (STArray s a)) -> Array a
{-# INLINE createArray #-}
createArray ma = runST $ do
  (width, v) <- ma
  frozen <- V.unsafeFreeze v
  pure (width, frozen)

widthArray' :: STArray s a -> Int
{-# INLINE widthArray' #-}
widthArray' (width, _) = width

heightArray' :: V.Unbox a => STArray s a -> Int
{-# INLINE heightArray' #-}
heightArray' (width, v) = VM.length v `div` width

indexArray :: V.Unbox a => Array a -> Int -> Int -> a
{-# INLINE indexArray #-}
indexArray (width, v) x y = V.unsafeIndex v (x + y * width)

widthArray :: Array a -> Int
{-# INLINE widthArray #-}
widthArray (width, _) = width

heightArray :: V.Unbox a => Array a -> Int
{-# INLINE heightArray #-}
heightArray (width, v) = V.length v `div` width

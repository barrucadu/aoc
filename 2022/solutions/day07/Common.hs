{-# LANGUAGE BangPatterns #-}

module Common where

import           Data.List (foldl')
import qualified Data.Map  as M

import           Utils     (stepParseInt)

type Filesystem = M.Map Path [(String, Entry)]

-- reverse order (/foo/bar == ["bar", "foo"])
type Path = [String]

data Entry = File Int | Dir

parse :: String -> Filesystem
parse = fst . foldl' go (M.singleton [] [], []) . lines where
  go :: (Filesystem, Path) -> String -> (Filesystem, Path)
  go (fs, cwd) "$ ls" = (fs, cwd)
  go (fs, cwd) ('$':' ':'c':'d':' ':dirname) = case dirname of
    "/" -> (fs, [])
    ".." -> (fs, tail cwd)
    _ -> (M.insert (dirname:cwd) [] fs, dirname:cwd)
  go (fs, cwd) ('d':'i':'r':' ':dirname) = let entry = (dirname, Dir) in (M.adjust (entry:) cwd fs, cwd)
  go (fs, cwd) l = let entry = parseFile 0 l in (M.adjust (entry:) cwd fs, cwd)

  parseFile !n (' ':fname) = (fname, File n)
  parseFile !n (x:xs) = parseFile (stepParseInt n x) xs

totalSize :: Filesystem -> Path -> Int
totalSize fs cwd = sum . map sizeOf $ M.findWithDefault [] cwd fs where
  sizeOf (_, File sz) = sz
  sizeOf (dir, Dir) = totalSize fs (dir:cwd)

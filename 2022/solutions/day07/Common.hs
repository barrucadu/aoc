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

directorySizes :: Filesystem -> M.Map Path Int
directorySizes fs = go M.empty (M.keys fs) where
  go :: M.Map Path Int -> [Path] -> M.Map Path Int
  go sizes (path:paths) =
    let (_, sizes') = go' sizes path
    in go sizes' paths
  go sizes [] = sizes

  go' :: M.Map Path Int -> Path -> (Int, M.Map Path Int)
  go' sizes path = case M.lookup path sizes of
    Just sz -> (sz, sizes)
    Nothing -> go'' sizes path 0 (M.findWithDefault [] path fs)

  go'' :: M.Map Path Int -> Path -> Int -> [(String, Entry)] -> (Int, M.Map Path Int)
  go'' sizes cwd !sz [] = (sz, M.insert cwd sz sizes)
  go'' sizes cwd !sz ((_, File fsz):es) = go'' sizes cwd (sz + fsz) es
  go'' sizes cwd !sz ((dir, Dir):es) =
    let (dsz, sizes') = go' sizes (dir:cwd)
    in go'' sizes' cwd (sz + dsz) es

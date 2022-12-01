{-# LANGUAGE BangPatterns #-}

import           Utils

main :: IO ()
main = mainFor 4 parse show

parse :: String -> Int
parse = go 0 [] where
  go :: Int -> String -> String -> Int
  go !acc passport ('\n':'\n':ls) = go (if validate passport then 1 + acc else acc) [] ls
  go !acc passport ('\n':[]) = if validate passport then 1 + acc else acc
  go !acc passport (l:ls) = go acc (l:passport) ls
  go !acc passport [] = if validate passport then 1 + acc else acc

  validate :: String -> Bool
  validate = validate' False False False False False False False where
    validate' byr iyr eyr hgt hcl ecl pid cs0 = case cs0 of
      (':':'r':'y':'b':cs) -> validate' True iyr eyr hgt hcl ecl pid cs
      (':':'r':'y':'i':cs) -> validate' byr True eyr hgt hcl ecl pid cs
      (':':'r':'y':'e':cs) -> validate' byr iyr True hgt hcl ecl pid cs
      (':':'t':'g':'h':cs) -> validate' byr iyr eyr True hcl ecl pid cs
      (':':'l':'c':'h':cs) -> validate' byr iyr eyr hgt True ecl pid cs
      (':':'l':'c':'e':cs) -> validate' byr iyr eyr hgt hcl True pid cs
      (':':'d':'i':'p':cs) -> validate' byr iyr eyr hgt hcl ecl True cs
      (_:cs) -> validate' byr iyr eyr hgt hcl ecl pid cs
      [] -> byr && iyr && eyr && hgt && hcl && ecl && pid

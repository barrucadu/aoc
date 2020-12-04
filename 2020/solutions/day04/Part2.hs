import Data.Char (isDigit, isHexDigit)

import Utils

main :: IO ()
main = mainFor 4 parse (show . solve)

data Passport = Passport
  { pByr :: String
  , pIyr :: String
  , pEyr :: String
  , pHgt :: String
  , pHcl :: String
  , pEcl :: String
  , pPid :: String
  }
  deriving Show

parse :: String -> [Passport]
parse = go [] [] where
  go :: [Passport] -> String -> String -> [Passport]
  go acc passport ('\n':'\n':ls) = go (parsePassport passport : acc) [] ls
  go acc passport ('\n':[]) = parsePassport passport : acc
  go acc passport (l:ls) = go acc (l:passport) ls
  go acc passport [] = parsePassport passport : acc

  parsePassport :: String -> Passport
  parsePassport = parsePassport' "" "" "" "" "" "" "" . reverse where
    parsePassport' byr iyr eyr hgt hcl ecl pid cs0 = case cs0 of
      ('b':'y':'r':':':cs) -> let (val, cs') = parseField cs in parsePassport' val iyr eyr hgt hcl ecl pid cs'
      ('i':'y':'r':':':cs) -> let (val, cs') = parseField cs in parsePassport' byr val eyr hgt hcl ecl pid cs'
      ('e':'y':'r':':':cs) -> let (val, cs') = parseField cs in parsePassport' byr iyr val hgt hcl ecl pid cs'
      ('h':'g':'t':':':cs) -> let (val, cs') = parseField cs in parsePassport' byr iyr eyr val hcl ecl pid cs'
      ('h':'c':'l':':':cs) -> let (val, cs') = parseField cs in parsePassport' byr iyr eyr hgt val ecl pid cs'
      ('e':'c':'l':':':cs) -> let (val, cs') = parseField cs in parsePassport' byr iyr eyr hgt hcl val pid cs'
      ('p':'i':'d':':':cs) -> let (val, cs') = parseField cs in parsePassport' byr iyr eyr hgt hcl ecl val cs'
      (_:cs) -> parsePassport' byr iyr eyr hgt hcl ecl pid cs
      [] -> Passport byr iyr eyr hgt hcl ecl pid

  parseField :: String -> (String, String)
  parseField = parseField' [] where
    parseField' acc (' ':cs) = (reverse acc, cs)
    parseField' acc ('\n':cs) = (reverse acc, cs)
    parseField' acc (c:cs) = parseField' (c:acc) cs
    parseField' acc [] = (reverse acc, [])

solve :: [Passport] -> Int
solve = length . filter validate where
  validate passport =
    validateDate 1920 2002 (pByr passport) &&
    validateDate 2010 2020 (pIyr passport) &&
    validateDate 2020 2030 (pEyr passport) &&
    validateHeight (pHgt passport) &&
    validateColourHex (pHcl passport) &&
    validateColourName (pEcl passport) &&
    validatePassportId (pPid passport)

  validateDate from to ds =
    let parsed = parseInt ds
    in length ds == 4 && all isDigit ds && parsed >= from && parsed <= to

  validateHeight [d1, d2, d3, 'c', 'm'] =
    let parsed = parseInt [d1, d2, d3]
    in parsed >= 150 && parsed <= 193
  validateHeight [d1, d2, 'i', 'n'] =
    let parsed = parseInt [d1, d2]
    in parsed >= 59 && parsed <= 76
  validateHeight _ = False

  validateColourHex ('#':ds) =
    length ds == 6 && all isHexDigit ds
  validateColourHex _ = False

  validateColourName col =
    col `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

  validatePassportId ds =
    length ds == 9 && all isDigit ds

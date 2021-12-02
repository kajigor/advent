module D04 where

import Data.List (sortBy)
import Data.Char (isDigit)
import Text.Read (readMaybe)

parse :: String -> [[(String, String)]]
parse xs =
  let ls = lines xs in
  let passports = split "" ls in
  map (parsePass . unlines) passports

split _ [] = []
split c xs =
  case span (/= c) xs of
    (x, "":y) -> x : split c y
    (x,_) -> [x]


parsePass xs =
  let fields = words xs in
  map (\x -> let (k,_:v) = span (/=':') x in (k,v)) fields

sortFields :: Ord a => [(a, a)] -> [(a, a)]
sortFields = sortBy (\(x,_) (y,_) -> compare x y)

obligatoryFields :: [String]
obligatoryFields =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  ]

optionalField :: String
optionalField = "cid"

areFieldsValid :: [(String, String)] -> Bool
areFieldsValid dict =
  all (\(k,_) -> k `elem` obligatoryFields || k == optionalField) dict &&
  all (\f -> (f `elem` map fst dict) || f == optionalField) obligatoryFields

task1 :: String -> Int
task1 input =
    let passports = parse input in
    let sorted = map sortFields passports in
    length $ filter areFieldsValid sorted

isPassValid :: [(String, String)] -> Bool
isPassValid dict =
  areFieldsValid dict &&
  all isFieldValid dict

between :: Int -> Int -> Int -> Bool
between lb x ub =
  lb <= x && x <= ub

isValidYear :: String -> Int -> Int -> Bool
isValidYear x lb ub =
  case readMaybe x of
    Just y -> between lb y ub
    Nothing -> False

isValidSequenceOf :: [Char] -> Int -> String -> Bool
isValidSequenceOf acceptableChars len xs =
  length xs == len && all (`elem` acceptableChars) xs

isFieldValid :: (String,String) -> Bool
isFieldValid ("byr", x) =
  isValidYear x 1920 2002
isFieldValid ("iyr", x) =
  isValidYear x 2010 2020
isFieldValid ("eyr", x) =
  isValidYear x 2020 2030
isFieldValid ("hgt", x) =
  let (value, unit) = span isDigit x in
  case readMaybe value of
    Just v ->
      case unit of
        "cm" -> between 150 v 193
        "in" -> between 59  v 76
        _ -> False
    _ -> False
isFieldValid ("hcl", '#':y) =
  isValidSequenceOf (['0'..'9'] ++ ['a'..'f']) 6 y
isFieldValid ("ecl", y) =
  y `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isFieldValid ("pid", y) =
  isValidSequenceOf ['0'..'9'] 9 y
isFieldValid ("cid", _) = True
isFieldValid _ = False


task2 :: String -> Int
task2 input =
    let passports = parse input in
    let sorted = map sortFields passports in
    length $ filter isPassValid sorted
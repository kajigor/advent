module Adv04 
  ( numOfPasswords
  , numOfPasswords2
  , adjacentDigits 
  , ascendingSequence
  , getDigits 
  , isPassword 
  , allPasswords
  , adjascentDigits2 
  , isPassword2
  ) where 

import Data.List (group)
import Debug.Trace 

adjacentDigits :: [Int] -> Bool
adjacentDigits = any (\x -> length x >= 2) . group

adjascentDigits2 :: [Int] -> Bool 
adjascentDigits2 xs = 
  any (\x -> length x == 2) $ group xs 

ascendingSequence :: [Int] -> Bool 
ascendingSequence xs = 
  all (uncurry (<=)) $ zip xs (tail xs)

getDigits :: Int -> [Int]
getDigits = reverse . go where
  go 0 = [] 
  go x = x `mod` 10 : go (x `div` 10)

isPassword :: Int -> Bool 
isPassword x = 
  let digits = getDigits x in 
  adjacentDigits digits && ascendingSequence digits 

isPassword2 :: Int -> Bool 
isPassword2 x = 
  let digits = getDigits x in 
  adjascentDigits2 digits && ascendingSequence digits

allPasswords :: Int -> Int -> [Int]
allPasswords lower upper = 
  [ x | x <- [lower .. upper], isPassword x ]

allPasswords2 :: Int -> Int -> [Int]
allPasswords2 lower upper = 
  [ x | x <- [lower .. upper], isPassword2 x ]

numOfPasswords :: Int -> Int -> Int 
numOfPasswords x = length . allPasswords x 

numOfPasswords2 :: Int -> Int -> Int 
numOfPasswords2 x = length . allPasswords2 x 
    
{-# LANGUAGE ScopedTypeVariables #-}

module Main where 

import Adv04 (numOfPasswords, numOfPasswords2)

input = "app/04/input"

main :: IO () 
main = do 
  file <- readFile input 
  let [lower, upper] :: [Int] = map read $ lines file 
  print $ numOfPasswords lower upper 
  print $ numOfPasswords2 lower upper 

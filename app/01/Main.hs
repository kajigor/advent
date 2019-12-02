module Main where

import Adv01 (fuel, allFuel)
  
input = "app/01/input"
  
main :: IO () 
main = do 
  input <- readFile input
  print $ sum $ map (fuel . read) $ lines input
  print $ sum $ map (allFuel . read) $ lines input

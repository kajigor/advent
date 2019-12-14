module Main where 

import Adv07 (findBiggestOutput, findBiggestOutput2)
import Util (split)

input = "app/07/input"

main :: IO () 
main = do
  file <- readFile input 
  let prog = split ',' file  
  print $ findBiggestOutput prog 0 [0 .. 4]
  print $ findBiggestOutput2 prog 0 [5 .. 9]



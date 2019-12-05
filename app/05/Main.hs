module Main where 

import Adv05 (runProg)
import Util

input = "app/05/input"

main :: IO () 
main = do 
  file <- readFile input
  let prog = split ',' file  
  print $ runProg prog [1]
  print $ runProg prog [5]
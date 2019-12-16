module Main where 

import Adv09 (runProg, runProg')
import Util (split)

input = "app/09/input"

main :: IO () 
main = do 
  file <- readFile input
  let prog = split ',' file  
  print $ runProg' prog [1]
  -- print $ runProg prog [5]
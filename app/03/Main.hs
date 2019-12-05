module Main where 

import Adv03 (solve)

input = "app/03/input"

main :: IO () 
main = do 
  input <- readFile input 
  let [first, second] = lines input 
  print $ solve first second 
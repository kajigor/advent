module Main where

import Adv02 (computeResult, update, findNounVerb)
import Util

input = "app/02/input"

main :: IO () 
main = do 
  input <- readFile input
  let items = map read $ split ',' input :: [Int]
  let modified = update items 12 2 
  print (computeResult modified)
  print (findNounVerb 19690720 items)
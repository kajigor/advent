module Main where 

import Adv16 (pattern, stage1, stage2) 
import Data.Char (digitToInt)

input = "app/16/input"

main :: IO () 
main = do 
  file <- readFile input 
  let xs = map digitToInt file
  putStrLn $ concatMap show $ stage1 xs
  putStrLn $ concatMap show $ stage2 xs

module Main where 

import Adv08 (solve, solve2, nicelyShow)

import Data.Char (digitToInt) 

input = "app/08/input"

main :: IO () 
main = do 
  file <- readFile input 
  print $ solve 25 6 $ map digitToInt file
  -- putStrLn $ nicelyShow 2 $ solve2 2 2 [0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0]
  putStrLn $ nicelyShow 25 $ solve2 25 6 $ map digitToInt file

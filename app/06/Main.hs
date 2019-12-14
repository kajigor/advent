module Main where 

import Adv06 (numberOfOrbits, parseOrbits, findShortestPath)

input = "app/06/input"

main :: IO () 
main = do 
  file <- readFile input 
  let orbits = parseOrbits file 
  print $ numberOfOrbits orbits  
  print $ findShortestPath orbits
module Test.Adv06 where 

import Adv06 
  ( makeTree
  , OrbitTree
  , numberOfOrbits
  , count
  , parseOrbits
  , unforgetfulMakeTree
  , findShortestPath
  , shortestPath
  ) 

import Test.HUnit (Assertion, (@?=))
import Data.Tree (Tree (..))
  
test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output  

test'' :: (Eq d, Show d) => (a -> b -> c -> d) -> a -> b -> c -> d -> Assertion
test'' f arg1 arg2 arg3 output = f arg1 arg2 arg3 @?= output

inputFolder :: String 
inputFolder = "test/Test/resources/06/"

com = "COM"

tree0 :: OrbitTree 
tree0 = Node "f" [
          Node "e" [],
          Node "c" [
            Node "a" [
              Node "b" [], 
              Node "d" []]]]

getOrbits :: String -> IO [(String, String)] 
getOrbits fileName = do 
  file <- readFile (inputFolder ++ fileName) 
  return $ parseOrbits file

getTree :: String -> IO OrbitTree 
getTree fileName = do 
  orbits <- getOrbits fileName 
  return $ makeTree com orbits

unit_makeTree :: Assertion
unit_makeTree = do 
  test (makeTree "f") [("a", "b"), ("f", "e"), ("c", "a"), ("f", "c"), ("a", "d")]
                      tree0
            
unit_count :: Assertion
unit_count = do
  test (count 0) tree0 10

  tree <- getTree "01"
  test (count 0) tree 42

  tree <- getTree "02"
  test (count 0) tree 54

ignoreFirst :: (a, b, c) -> (b, c)
ignoreFirst (_,x,y) = (x,y)

unit_unforgetfulMakeTree :: Assertion
unit_unforgetfulMakeTree = do
  let testFunction x = ignoreFirst <$> unforgetfulMakeTree com x 
  
  orbits <- getOrbits "01"
  test testFunction orbits Nothing

  orbits <- getOrbits "02"
  test testFunction orbits (Just ("K", "I"))

unit_findShortestPath :: Assertion 
unit_findShortestPath = do 
  orbits <- getOrbits "01"
  test findShortestPath orbits Nothing

  orbits <- getOrbits "02"
  test findShortestPath orbits (Just 4)

unit_shortestPath :: Assertion 
unit_shortestPath = do  
  tree <- getTree "01"
  test'' shortestPath tree "Z" "D" Nothing 
  test'' shortestPath tree "B" "D" (Just 2) 
  test'' shortestPath tree "C" "D" (Just 1) 
  test'' shortestPath tree "D" "D" (Just 0) 
  test'' shortestPath tree "E" "D" (Just 1) 
  test'' shortestPath tree "F" "D" (Just 2) 
  test'' shortestPath tree "G" "D" (Just 3) 
  test'' shortestPath tree "H" "D" (Just 4) 
  test'' shortestPath tree "J" "D" (Just 2) 
  test'' shortestPath tree "K" "D" (Just 3) 
  test'' shortestPath tree "L" "D" (Just 4) 

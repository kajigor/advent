module Adv06 
  ( numberOfOrbits
  , makeTree
  , OrbitTree 
  , count 
  , parseOrbits 
  , unforgetfulMakeTree
  , findShortestPath
  , shortestPath
  ) where 

import Data.List (partition, find)
import Util (split)
import Data.Tree (Tree (..))
import Data.Maybe (catMaybes) 
import Debug.Trace (trace)

type OrbitTree = Data.Tree.Tree String

com :: String 
com = "COM"

parseOrbits :: String -> [(String, String)]
parseOrbits = map (\[x,y] -> (x, y)) . map (split ')') . lines

makeTree :: String -> [(String, String)] -> OrbitTree 
makeTree cur zs = 
  let (xs, ys) = partition ((== cur) . fst) zs in 
  Node cur (map (\x -> makeTree (snd x) ys) xs) 

count :: Int -> OrbitTree -> Int 
count depth (Node _ ch) = depth + sum (map (count (depth + 1)) ch) 

numberOfOrbits :: [(String, String)] -> Int
numberOfOrbits xs = 
  let tree = makeTree com xs in 
  count 0 tree  


you :: String 
you = "YOU" 

santa :: String 
santa = "SAN"
  
unforgetfulMakeTree :: String -> [(String, String)] -> Maybe (OrbitTree, String, String)
unforgetfulMakeTree cur zs = do
  yourParent  <- find ((== you)   . snd) zs 
  santaParent <- find ((== santa) . snd) zs 
  return (makeTree cur zs, fst yourParent, fst santaParent)

shortestPath :: OrbitTree -> String -> String -> Maybe Int 
shortestPath _ node1 node2 | node1 == node2 = Just 0
shortestPath tree node1 node2 = do 
  ancestors1 <- findAncestors tree node1 
  ancestors2 <- findAncestors tree node2 
  let len = length $ uncurry (++) $ dropCommonPref ancestors1 ancestors2 
  Just $ if node1 `elem` ancestors2 || node2 `elem` ancestors1
         then len 
         else (len + 2)
    where 
      findAncestors tree node = go tree node where
        go (Node x _) goal | x == goal = Just []
        go (Node _ []) _ = Nothing 
        go (Node x ch) goal = 
          let resInCh = map (flip go goal) ch in 
          case catMaybes resInCh of 
            [] -> Nothing 
            (r : _) -> Just (x:r)

      dropCommonPref [] ys = ([], ys) 
      dropCommonPref xs [] = (xs, [])
      dropCommonPref (x:xs) (y:ys) | x == y = dropCommonPref xs ys 
      dropCommonPref xs ys = (xs, ys) 
      
findShortestPath :: [(String, String)] -> Maybe Int 
findShortestPath xs = do 
  (tree, yourParent, santaParent) <- unforgetfulMakeTree com xs 
  shortestPath tree yourParent santaParent

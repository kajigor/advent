module Adv03 (
    solve
  , parseList
  , Instr (..)
  , Coord (..)
  , eval
  , coords
  ) where 

import Util (split)
import Data.List (intersect, sortBy)
import Debug.Trace (trace)

data Instr = R Int | L Int | U Int | D Int
           deriving (Show, Eq)

data Coord = Coord Int Int 
           deriving (Show, Eq, Ord)

center :: Coord 
center = Coord 0 0 

parse :: String -> Instr
parse (x:xs) =
  case x of 
    'R' -> R 
    'L' -> L
    'U' -> U
    'D' -> D
  $ read xs

parseList :: String -> [Instr]
parseList = map parse . split ','

up :: Coord -> Coord 
up (Coord i j) = Coord (i + 1) j

down :: Coord -> Coord 
down (Coord i j) = Coord (i - 1) j 

right :: Coord -> Coord 
right (Coord i j) = Coord i (j + 1)

left :: Coord -> Coord 
left (Coord i j) = Coord i (j - 1)

choose :: Instr -> (Coord -> Coord, Int) 
choose (R i) = (right, i)
choose (L i) = (left,  i)
choose (U i) = (up,    i)
choose (D i) = (down,  i)

eval :: Coord -> Instr -> [Coord]
eval coord instr = 
  let (fun, i) = choose instr in 
  reverse $ coord : go fun i coord 
    where 
      go _ 0 _ = [] 
      go f n c = let c' = f c in c' : go f (n - 1) c'

coords :: [Instr] -> [Coord]
coords = 
  trace "coords" $
  go center where 
    go coord [] = [coord]
    go coord (i:is) = 
      let coords = eval coord i in 
      tail coords ++ go (head coords) is   
  
distToCenter :: Coord -> Int
distToCenter (Coord x y) = abs x + abs y


solve :: String -> String -> Int
solve first second = 
  let fInsts = parseList first in 
  let sInsts = parseList second in 
  let fCoords = coords fInsts in 
  let sCoords = coords sInsts in
  let common = intersect fCoords sCoords in 
  distToCenter $ head $ tail $ sortBy (\x y -> distToCenter x `compare` distToCenter y) common 

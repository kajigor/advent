module Adv08 where 

import Data.List (sortBy, transpose)
import Data.List.Split (divvy)
import Debug.Trace

splitInLayers :: Int -> Int -> [a] -> [[a]]
splitInLayers w l xs = 
  let layerLength = w * l in  
  divvy layerLength layerLength xs 

solve :: Int -> Int -> [Int] -> Int 
solve width len xs =
  let layers = splitInLayers width len xs in
  let numberOf0 = length . filter (== 0) in 
  let smallest0 = head $ sortBy (\x y -> numberOf0 x `compare` numberOf0 y) layers in 
  let ones = filter (== 1) smallest0 in  
  let twos = filter (== 2) smallest0 in 
  length ones * length twos

topMost :: [Int] -> Int
topMost [x] = x 
topMost (2:xs) = topMost xs 
topMost (x:_) = x 

solve2 :: Int -> Int -> [Int] -> [Int]
solve2 width len xs = 
  let layers = splitInLayers width len xs in
  let transposed = transpose layers in 
  trace (show transposed) $ 
  map topMost transposed 

nicelyShow :: Show a => Int -> [a] -> String
nicelyShow w xs = 
  let layers = splitInLayers w 1 xs in 
  unlines $ map (concatMap show) layers
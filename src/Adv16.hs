module Adv16 ( 
    basePattern
  , pattern 
  , vectorProd
  , phase 
  , hudreedPhases
  , stage1
  , stage2
  ) where

basePattern :: [Int] 
basePattern = [0, 1, 0, -1]

pattern :: Int -> [Int] 
pattern n = 
  tail $ concat $ repeat $ concatMap (replicate n) basePattern 

vectorProd :: [Int] -> [Int] -> Int
vectorProd xs pat = 
  (`mod` 10) $ abs $  sum $ zipWith (*) xs pat 

phase :: [Int] -> [Int]
phase input = 
  map (vectorProd input) [pattern n | n <- [1 .. length input]] 

hudreedPhases :: [Int] -> [Int]
hudreedPhases = go 100
  where 
    go 0 xs = xs 
    go n xs = go (n - 1) (phase xs) 

stage1 :: [Int] -> [Int]
stage1 = take 8 . hudreedPhases

stage2 :: [Int] -> [Int]
stage2 xs = 
  let offset = foldl1 (\acc x -> acc * 10 + x) $ take 7 xs  in
  take 8 $ drop offset $ hudreedPhases $ concat $ replicate 10000 xs
   

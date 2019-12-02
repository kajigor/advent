module Adv01
  ( fuel
  , allFuel
  ) where

fuel :: Int -> Int 
fuel x = (x `div` 3) - 2 

fix :: (Ord a, Num a) => (a -> a) -> a -> a -> a 
fix f x acc | x <= 0 = acc 
fix f x acc = fix f (f x) (x + acc)

allFuel :: Int -> Int 
allFuel x = fix fuel x 0 - x 
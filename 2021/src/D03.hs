module D03 where

import Data.List (transpose)
import Debug.Trace (trace)

task1 :: String -> Int
task1 xs =
    let nums = map readChar $ lines xs in
    let transposed = transpose nums in
    let counted = map (common . count (0,0)) transposed in
    let (gamma, epsilon) = unzip counted in
    toInt gamma * toInt epsilon

count :: (Eq a1, Num a1, Num a2, Num a3) => (a2, a3) -> [a1] -> (a2, a3)
count (x, y) [] = (x, y)
count (x, y) (0 : xs) = count (x + 1, y) xs
count (x, y) (1 : xs) = count (x, y + 1) xs
count _ _ = error "impossible case"

mostCommon :: (Ord a, Num p) => (a, a) -> p
mostCommon (x, y) | x > y = 0
                  | otherwise = 1

leastCommon :: (Ord a, Num p) => (a, a) -> p
leastCommon (x, y) | x > y = 1
                   | otherwise = 0

common :: (Ord a1, Num a2, Num b) => (a1, a1) -> (a2, b)
common (x, y) | x > y = (0, 1)
              | otherwise = (1, 0)

readChar :: [Char] -> [Int]
readChar = map toNum

toNum :: Char -> Int
toNum '0' = 0
toNum '1' = 1
toNum _ = error "impossible case"

toInt :: [Int] -> Int
toInt = foldl1 (\x y -> 2 * x + y)

task2 :: String -> Int
task2 xs =
    let nums = map readChar $ lines xs in
    let oxygenRating = calculateRating mostCommon nums [] in
    let co2Rating = calculateRating leastCommon nums [] in
    toInt oxygenRating * toInt co2Rating
  where
    filterNumbers _ [[x]] = (x, [])
    filterNumbers p xs =
      let bitFilter = p $ count (0,0) $ map head xs in
      (bitFilter, map tail $ filter (\x -> bitFilter == head x) xs)
    calculateRating _ [x] acc = reverse $ reverse x ++ acc
    calculateRating p xs acc =
      let (x, next) = filterNumbers p xs in
      calculateRating p next (x:acc)



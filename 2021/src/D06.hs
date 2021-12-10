module D06 where

import qualified Data.Map as M
import Data.List (group, sort)

parseInput :: String -> [Int]
parseInput xs =
    let nums = splitBy (',') xs in
    map read nums
  where
    splitBy elem l =
      case span (/= elem) l of
        (x, []) -> [x]
        (x, (_:xs)) -> x : splitBy elem xs

-- solution by definition
task1 :: String -> Int
task1 str =
    let fish = parseInput str in
    length $ go 80 fish
  where
    go 0 fish = fish
    go n fish = go (n-1) (step 0 fish)
    step n [] = replicate n 8
    step n (x:xs) | x > 0 = (x - 1 : step n xs)
                  | otherwise = (6 : step (n + 1) xs)

-- solution via Maps
task2 :: String -> Int
task2 str =
    let parsed = parseInput str in
    let fish = M.fromList $ [(x, y) | x <- [0..8], let y = length $ filter (==x) parsed] in
    sum $ M.elems (iterate step fish !! 256)
  where
    step m =
        M.insertWith (+) 8 new next
      where
        new = M.findWithDefault 0 0 m
        step k = if k > 0 then k - 1 else 6
        next = M.mapKeysWith (+) step m
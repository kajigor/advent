module D06 where

import Data.List.Split
import qualified Data.Set as Set
import Data.List

task1 :: String -> Int
task1 xs =
  let groups = concat . lines <$> splitOn "\n\n" xs in
  let sets = map Set.fromList groups in
  sum $ map length sets

task2 :: String -> Int
task2 xs =
    sum $ countGroup . lines <$> splitOn "\n\n" xs
  where
    countGroup ans =
      let numOfPeople = length ans in
      let groups = group $ sort $ concat ans in
      length $ filter ((numOfPeople ==) . length) groups

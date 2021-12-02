module D05 where

import Data.List

task1 :: [String] -> Int
task1 xs =
    head $ reverse $ sort $ map seatID xs

seatID xs =
  let (row, column) = splitAt 7 xs in
  let rowNum = map (\x -> if x == 'F' then 0 else 1) row in
  let columnNum = map (\x -> if x == 'L' then 0 else 1) column in
  (binToInt rowNum) * 8 + (binToInt columnNum)

binToInt =
    go . reverse
  where
    go [] = 0
    go (h:t) = h + 2 * (go t)

task2 :: [String] -> Int
task2 xs =
    case find (\(x,y) -> y - x == 2) $ makePairs $ sort $ map seatID xs of
      Just (x,y) -> (x + y) `div` 2
      _ -> error "Are you sure you are on this plane?"

makePairs (x:y:t) = (x,y) : makePairs (y:t)
makePairs _ = []


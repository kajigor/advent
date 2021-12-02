module D01 where

task1 xs =
  length $ filter (\(x, y) -> x < y) $ zip xs $ tail xs

task2 xs =
  let windows = zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail $ tail xs)
  in task1 windows


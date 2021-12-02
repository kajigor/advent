module D03 where

task1 :: [String] -> Int
task1 xs =
    length $ filter (\(i,line) -> '#' == (line !! (i*3))) $ zip [0..] (map cycle xs)

countTrees xs right down  =
  length $ filter (=='#') $ [cycle (xs !! d) !! r | (r, d) <- zip [0, right ..] [0, down .. length xs -1]]

task2 xs =
    product $ [countTrees xs right down| (right, down) <- [(1,1),(3,1),(5,1),(7,1),(1,2)]]

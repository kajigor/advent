module D01 where

task1 xs =
  let [x, y] = head $ filter ((2020 ==) . sum ) [[x,y] | x <- xs, y <- xs]
  in  x * y

task2 xs =
  let [x, y, z] = head $ filter ((2020 ==) . sum ) [[x,y,z] | x <- xs, y <- xs, z <- xs]
  in  x * y * z


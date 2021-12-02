module D02 where

task1 xs =
    length $ filter (==True) $ map isValidPass xs
  where
    isValidPass (lb, ub, c, xs) =
      let len = length $ filter (==c) xs in
      lb <= len && len <= ub

task2 xs =
    length $ filter (==True) $ map isValidPass xs
  where
    isValidPass (lb, ub, c, xs) =
      let lbc = xs !! (lb - 1) in
      let ubc = xs !! (ub - 1) in
      (lbc == c) `xor` (ubc == c)
    xor True False = True
    xor False True = True
    xor _ _ = False
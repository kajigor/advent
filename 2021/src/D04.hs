module D04 where

import Debug.Trace ( trace )
import Data.List ( transpose, find, delete, last, (\\), intercalate )

type Board = [[(Int, Bool)]]

parseInput :: String -> ([Int], [Board])
parseInput xs =
    let (l : "" : ls) = lines xs in
    let nums = splitBy (',') l in
    let boards = splitBy ("") ls in
    (map read nums, map toBoard boards)
  where
    splitBy elem l =
      case span (/= elem) l of
        (x, []) -> [x]
        (x, (_:xs)) -> x : splitBy elem xs
    toBoard = map (map (\x -> (read x, False)) . words)


mark :: Int -> Board -> Board
mark n =
  map (map (\(x, m) -> (x, x == n || m)))

check :: Board -> Bool
check b =
    any checkLine (rows b) || any checkLine (columns b)
  where
    checkLine :: [(Int, Bool)] -> Bool
    checkLine line = all snd line
    rows b = b
    columns b = transpose b

postProcess :: (Int, Board) -> Int
postProcess (n, b) =
  let unmarked = map fst $ filter (not . snd) (concat b) in
  sum unmarked * n


task1 :: String -> Int
task1 input =
    let (nums, boards) = parseInput input in
    postProcess $ go nums boards
  where
    go :: [Int] -> [Board] -> (Int, Board)
    go (n : ns) boards =
      let boards' = map (mark n) boards in
      case find check boards' of
        Nothing -> go ns boards'
        Just b -> (n, b)
    go [] _ = undefined

showBoard :: Board -> String
showBoard xs =
    intercalate "\n" $ map (unwords . map showPair) xs
  where
    showPair x =
        let shown = go x in
        (if length shown == 3 then " " else "") ++ shown
      where
        go (x, True) = "[" ++ show x ++ "]"
        go (x, False) = " " ++ show x ++ " "

task2 :: String -> Int
task2 input =
    let (nums, boards) = parseInput input in
    let last = go nums boards (-1, []) in
    postProcess last
  where
    go :: [Int] -> [Board] -> (Int, Board) -> (Int, Board)
    go (n : ns) boards lst =
      let boards' = map (mark n) boards in
      trace ("\n================\n" ++ show n ++ "\n" ++ (intercalate "\n\n" $ map showBoard boards')) $
      case filter check boards' of
        [] -> go ns boards' lst
        bs -> go ns (boards' \\ bs) (n, last bs)
    go [] _ last = last

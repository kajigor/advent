module Main where

import qualified D06
import Data.List

baseDir :: String
baseDir = "app/input/"

readListInt :: String -> IO [Int]
readListInt fileName = do
  file <- readFile (baseDir ++ fileName)
  return $ map read $ words file

readLines :: String -> IO [String]
readLines fileName =
  lines <$> readFile (baseDir ++ fileName)

readString :: String -> IO String
readString fileName =
  readFile (baseDir ++ fileName)

main :: IO ()
main = do
  input <- readString "06.txt"
  print (D06.task1 input)
  print (D06.task2 input)


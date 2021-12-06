module Main where

import qualified D04
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
  input <- readString "04.txt"
  print (D04.task1 input)
  print (D04.task2 input)


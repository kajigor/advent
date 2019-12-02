{-# LANGUAGE RankNTypes #-}

module Adv02 
  ( computeResult
  , update
  , findNounVerb
  , runProg
  , EvalRes (..)
  ) where 

import Data.List (find)
import Data.Maybe (isJust, fromJust)

data EvalRes a = Halt [a]
               | Error 
               | Success [a]
               deriving (Show, Eq)
  

evalCmd :: [Int] -> Int -> EvalRes Int
evalCmd v idx = 
  case v !! idx of 
    1  -> Success $ runFunction v idx (+) 
    2  -> Success $ runFunction v idx (*) 
    99 -> Halt v
    _  -> Error 
    where 
      runFunction v idx f = 
        let i  = v !! (idx + 1) in
        let j  = v !! (idx + 2) in
        let k  = v !! (idx + 3) in
        let ai = v !! i in
        let aj = v !! j in
        let (ls, (_:rs)) = splitAt k v in   
        ls ++ (f ai aj) : rs 

runProg :: [Int] -> EvalRes Int
runProg xs = go xs 0 where 
  go xs idx = 
    case evalCmd xs idx of 
      Success xs' -> go xs' (idx + 4)
      x -> x 

computeResult :: [Int] -> Maybe Int
computeResult xs = 
  case runProg xs of 
    Halt res -> return $ head res 
    _ -> Nothing

update :: [a] -> a -> a -> [a]
update xs a b = head xs : a : b : (tail $ tail $ tail xs)

findNounVerb :: Int -> [Int] -> Maybe Int 
findNounVerb goal items = 
  let allResults = [(n, v, computeResult (update items n v)) | n <- [0 .. 99], v <- [0 .. 99]] in
  (\(noun, verb, _) -> noun * 100 + verb) <$> 
    find (\(_,_, r) -> isJust r && fromJust r == goal) allResults
  
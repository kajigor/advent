module Adv07 where 

import Adv05 (runProg, runProg')
import Data.List (permutations, maximum)
import Debug.Trace (trace)

findBiggestOutput :: [String] -> Int -> [Int] -> Int 
findBiggestOutput prg input ampPhases = 
  let amplifiers = permutations ampPhases in 
  let allVariants = map (foldl (\i p -> runProg prg [p, i]) input) amplifiers in 
  maximum allVariants

findBiggestOutput2 :: [String] -> Int -> [Int] -> Int 
findBiggestOutput2 prg input ampPhases = 
  let amplifiers = permutations ampPhases in 
  let allVariants = map go amplifiers in 
  maximum (map last allVariants)
    where 
      go (a:b:c:d:e:_) = 
        let resA = runProg' prg (a:0:resE) 
            resB = runProg' prg (b:resA)
            resC = runProg' prg (c:resB)
            resD = runProg' prg (d:resC) 
            resE = runProg' prg (e:resD) in
        -- trace ( "resE: " ++ show resE )  $ 
        resE
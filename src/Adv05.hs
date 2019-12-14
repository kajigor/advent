module Adv05 
  ( runProg 
  , runProg'
  , parseInstr 
  , Mode (..)
  , Instr (..)
  , Input 
  , Output
  , Memory
  , ProgramCounter
  , eval 
  ) where 

import Data.Char (digitToInt)

data Mode = Position | Intermediate 
          deriving (Eq, Show)

type Input = [Int]
type Output = [Int] 
type Memory = [Int]
type ProgramCounter = Int 

data Instr = Add Mode Mode 
           | Mul Mode Mode 
           | JumpIfTrue Mode Mode 
           | JumpIfFalse Mode Mode 
           | LessThan Mode Mode
           | Equal Mode Mode 
           | Read Mode 
           | Write Mode 
           | Halt
           deriving (Eq, Show)

toMode :: Int -> Mode 
toMode 0 = Position 
toMode 1 = Intermediate 

parseInstr :: Int -> Instr 
parseInstr inp = 
  case inp of 
    99 -> Halt 
    _  -> 
      let (x:y:z:t:_) = (reverse (map digitToInt $ show inp)) ++ [0,0,0,0] in 
      case x of 
        1 -> Add (toMode z) (toMode t)
        2 -> Mul (toMode z) (toMode t)
        3 -> Read (toMode z)
        4 -> Write (toMode z)
        5 -> JumpIfTrue (toMode z) (toMode t)
        6 -> JumpIfFalse (toMode z) (toMode t)
        7 -> LessThan (toMode z) (toMode t)
        8 -> Equal (toMode z) (toMode t)
        _ -> error $ "Nonexistent opcode: " ++ show inp
          
eval :: ProgramCounter -> Input -> Memory -> Output -> (Output, Memory)
eval pc input memory output = 
  let (instr : arg1 : arg2 : arg3 : _) = (drop pc memory) ++ [0,0,0,0] in 
  case parseInstr instr of 
    Halt -> (output, memory)
    Read mode1 -> 
      let memory' = modify arg1 (head input) in 
      eval (pc + 2) (tail input) memory' output 
    Write mode1 -> 
      let value = getValue arg1 mode1 in 
      eval (pc + 2) input memory (value : output)
    Add mode1 mode2 -> 
      let memory' = runFunction mode1 mode2 (+) arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output
    Mul mode1 mode2 -> 
      let memory' = runFunction mode1 mode2 (*) arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output 
    JumpIfTrue mode1 mode2 -> 
      jump isTrue mode1 mode2 arg1 arg2 
    JumpIfFalse mode1 mode2 -> 
      jump isFalse mode1 mode2 arg1 arg2 
    LessThan mode1 mode2 -> 
      let memory' = cond (<) mode1 mode2 arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output 
    Equal mode1 mode2 -> 
      let memory' = cond (==) mode1 mode2 arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output
    where 
      runFunction :: Mode -> Mode -> (Int -> Int -> Int) -> Int -> Int -> Int -> Memory
      runFunction mode1 mode2 f arg1 arg2 arg3 = 
        let x = getValue arg1 mode1 in 
        let y = getValue arg2 mode2 in 
        modify arg3 (f x y) 

      getValue :: Int -> Mode -> Int 
      getValue arg Intermediate = arg
      getValue arg Position = memory !! arg

      modify :: Int -> Int -> Memory 
      modify arg value = 
        let (ls, (_:rs)) = splitAt arg memory in   
        ls ++ value : rs 
      
      jump :: (Int -> Bool) -> Mode -> Mode -> Int -> Int -> (Output, Memory) 
      jump pred mode1 mode2 arg1 arg2 = 
        let pc' = if pred $ getValue arg1 mode1 
                  then getValue arg2 mode2 
                  else pc + 3 
        in eval pc' input memory output 

      cond :: (Int -> Int -> Bool) -> Mode -> Mode -> Int -> Int -> Int -> Memory
      cond pred mode1 mode2 arg1 arg2 arg3 = 
        let x = getValue arg1 mode1 in 
        let y = getValue arg2 mode2 in 
        let res = if pred x y then 1 else 0 in
        modify arg3 res  

      isTrue :: Int -> Bool 
      isTrue = (/= 0)
    
      isFalse :: Int -> Bool 
      isFalse = (== 0)

runProg' :: [String] -> Input -> [Int] 
runProg' str input =
  let memory = map read str :: [Int] in 
  let pc = 0 in 
  let (output, _) = eval pc input memory [] in 
  output 

runProg :: [String] -> Input -> Int 
runProg prg = head . runProg' prg
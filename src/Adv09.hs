module Adv09
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
import Debug.Trace (trace) 
import Text.Printf (printf)
import Data.List (drop, take, intercalate)

data Mode = Position 
          | Intermediate  
          | Relative
          deriving (Eq, Show)

type Input = [Integer]
type Output = [Integer] 
type RelativeOffset = Integer 
type Memory = [Integer]
type ProgramCounter = Integer
type Result = (Output, Memory, RelativeOffset)

data Instr = Add Mode Mode 
           | Mul Mode Mode 
           | JumpIfTrue Mode Mode 
           | JumpIfFalse Mode Mode 
           | LessThan Mode Mode
           | Equal Mode Mode 
           | Read Mode 
           | Write Mode 
           | Halt
           | RelBaseOffset Mode 
           deriving (Eq, Show)

toMode :: Integer -> Mode 
toMode 0 = Position 
toMode 1 = Intermediate 
toMode 2 = Relative

showMemory :: Memory -> String 
showMemory mem = 
  let strings = map (\(i, x) -> printf "%s:\t%s" (show i) (show x)) (zip [0..length mem] mem) in 
  let lists = go 5 strings :: [[String]] in 
  unlines $ map (intercalate "\t") lists 
    where 
      go n [] = []
      go n xs = take n xs : (go n (drop n xs))

parseInstr :: Integer -> Instr 
parseInstr inp = 
  case inp of 
    99 -> Halt 
    _  -> 
      let (x:_:z:t:_) = (reverse (map (toInteger . digitToInt) $ show inp)) ++ [0,0,0,0] in 
      case x of 
        1 -> Add (toMode z) (toMode t)
        2 -> Mul (toMode z) (toMode t)
        3 -> Read (toMode z)
        4 -> Write (toMode z)
        5 -> JumpIfTrue (toMode z) (toMode t)
        6 -> JumpIfFalse (toMode z) (toMode t)
        7 -> LessThan (toMode z) (toMode t)
        8 -> Equal (toMode z) (toMode t)
        9 -> RelBaseOffset (toMode z)
        _ -> error $ "Nonexistent opcode: " ++ show inp
          
eval :: ProgramCounter -> Input -> Memory -> Output -> RelativeOffset -> Result
eval pc input memory output relOffset = 
  trace (printf "\nEval %s\nInput: %s\nOutput: %s\nRelOffset: %s\nMemory:\n%s\n" (show pc)  (show input) (show output) (show relOffset) (showMemory $ take 1005 memory)) $ 

  let (instr : arg1 : arg2 : arg3 : _) = (drop (fromInteger pc) memory) ++ [0,0,0,0] in 
  case parseInstr instr of 
    Halt -> (output, memory, relOffset)
    Read mode1 -> 
      let value = getValue arg1 mode1 relOffset in 
      let memory' = modify value (head input) in 
      trace (printf "Read\nValue: %s\n" (show value)) $
      eval (pc + 2) (tail input) memory' output relOffset 
    Write mode1 -> 
      let value = getValue arg1 mode1 relOffset in 
      eval (pc + 2) input memory (value : output) relOffset 
    Add mode1 mode2 -> 
      let memory' = runFunction mode1 mode2 (+) arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output relOffset
    Mul mode1 mode2 -> 
      let memory' = runFunction mode1 mode2 (*) arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output relOffset
    JumpIfTrue mode1 mode2 -> 
      jump isTrue mode1 mode2 arg1 arg2 relOffset 
    JumpIfFalse mode1 mode2 -> 
      jump isFalse mode1 mode2 arg1 arg2 relOffset 
    LessThan mode1 mode2 -> 
      let memory' = cond (<) mode1 mode2 arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output relOffset 
    Equal mode1 mode2 -> 
      let memory' = cond (==) mode1 mode2 arg1 arg2 arg3 in 
      eval (pc + 4) input memory' output relOffset 
    RelBaseOffset mode1 ->  
      let value = getValue arg1 mode1 relOffset in 
      trace (printf "RelBaseOffset!\nrelOffset: %s\nvalue: %s\n" (show relOffset) (show value)) $ 
      eval (pc + 2) input memory output (relOffset + value)
    where 
      runFunction :: Mode -> Mode -> (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Memory
      runFunction mode1 mode2 f arg1 arg2 arg3 = 
        let x = getValue arg1 mode1 relOffset in 
        let y = getValue arg2 mode2 relOffset in 
        modify arg3 (f x y) 

      getValue :: Integer -> Mode -> RelativeOffset -> Integer 
      getValue arg Intermediate _ = arg
      getValue arg Position _ = memory !! fromInteger arg
      getValue arg Relative relOffset = getValue (relOffset + arg) Position relOffset

      modify :: Integer -> Integer -> Memory 
      modify arg value = 
        let (ls, (_:rs)) = splitAt (fromInteger arg) memory in   
        ls ++ value : rs 
      
      jump :: (Integer -> Bool) -> Mode -> Mode -> Integer -> Integer -> RelativeOffset -> Result 
      jump pred mode1 mode2 arg1 arg2 relOffset = 
        let pc' = if pred $ getValue arg1 mode1 relOffset 
                  then getValue arg2 mode2 relOffset 
                  else pc + 3 
        in eval pc' input memory output relOffset

      cond :: (Integer -> Integer -> Bool) -> Mode -> Mode -> Integer -> Integer -> Integer -> Memory
      cond pred mode1 mode2 arg1 arg2 arg3 = 
        let x = getValue arg1 mode1 relOffset in 
        let y = getValue arg2 mode2 relOffset in 
        let res = if pred x y then 1 else 0 in
        modify arg3 res  

      isTrue :: Integer -> Bool 
      isTrue = (/= 0)
    
      isFalse :: Integer -> Bool 
      isFalse = (== 0)

runProg' :: [String] -> Input -> [Integer] 
runProg' str input =
  let memory = map read str ++ repeat 0 :: [Integer] in 
  let pc = 0 in 
  let relOffset = 0 in 
  let (output, _, _) = eval pc input memory [] relOffset in 
  output 

runProg :: [String] -> Input -> Integer  
runProg prg = head . runProg' prg
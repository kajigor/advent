module Test.Adv09 where 

import Adv09 (parseInstr, Mode (..), Instr (..), eval) 

import Test.HUnit (Assertion, (@?=))

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output  

fst3 :: (a, b, c) -> a 
fst3 (x, _, _) = x 

-- unit_parseInstr = do 
--   test parseInstr 3   (Read Position)
--   test parseInstr 4   (Write Position)
--   test parseInstr 104 (Write Intermediate)
--   test parseInstr 99   Halt 

--   test parseInstr 101  (Add Intermediate Position) 
--   test parseInstr 1101 (Add Intermediate Intermediate) 
--   test parseInstr 1001 (Add Position     Intermediate)
--   test parseInstr 1    (Add Position     Position) 

--   test parseInstr 102  (Mul Intermediate Position) 
--   test parseInstr 1102 (Mul Intermediate Intermediate) 
--   test parseInstr 1002 (Mul Position     Intermediate)
--   test parseInstr 2    (Mul Position     Position) 

-- unit_eval = do 
--   fst3 (eval 0 [123] [3, 0, 4, 0, 99] [] 0) @?= [123]
--   (eval 0 [] [1002,4,3,4,33] [] 0) @?= ([], [1002,4,3,4,99], 0)
--   (eval 0 [] [1101,100,-1,4,0] [] 0) @?= ([], [1101,100,-1,4,99], 0)

unit_eval_long = do
  -- let input = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  -- reverse (runEval input) @?= input

  -- let input = [1102,34915192,34915192,7,4,7,99,0]
  -- head (runEval input) @?= 1219070632396864

  -- let input = [104,1125899906842624,99]
  -- head (runEval input) @?= 1125899906842624

  -- let input = [203,2,0]
  -- (eval 0 [99] input [] 0) @?= ([], [203, 2, 99], 0) 

  let input = [9,1,203,3,4]
  (eval 0 [99] input [] 0) @?= ([], [9, 1, 203, 3, 99], 1) 


    where 
      runEval input = 
        take (length input) (fst3 (eval 0 [] (input ++ repeat 0) [] 0))

-- unit_eval_equal = do 
--   fst3 (eval 0 [8] [3,9,8,9,10,9,4,9,99,-1,8] [] 0) @?= [1]
--   fst3 (eval 0 [9] [3,9,8,9,10,9,4,9,99,-1,8] [] 0) @?= [0]
--   fst3 (eval 0 [8] [3,3,1108,-1,8,3,4,3,99] [] 0) @?= [1]
--   fst3 (eval 0 [9] [3,3,1108,-1,8,3,4,3,99] [] 0) @?= [0]

-- unit_eval_less = do 
--   fst3 (eval 0 [1] [3,9,7,9,10,9,4,9,99,-1,8] [] 0) @?= [1]
--   fst3 (eval 0 [9] [3,9,7,9,10,9,4,9,99,-1,8] [] 0) @?= [0]
--   fst3 (eval 0 [1] [3,3,1107,-1,8,3,4,3,99] [] 0) @?= [1]
--   fst3 (eval 0 [9] [3,3,1107,-1,8,3,4,3,99] [] 0) @?= [0]

-- unit_eval_jump = do 
--   fst3 (eval 0 [0] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [] 0) @?= [0]
--   fst3 (eval 0 [3] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [] 0) @?= [1]
--   fst3 (eval 0 [0] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [] 0) @?= [0]
--   fst3 (eval 0 [3] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [] 0) @?= [1]

-- unit_eval_larger = do 
--   fst3 (eval 0 [3] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
--                     1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
--                     999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [] 0) @?= [999]
--   fst3 (eval 0 [8] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
--                     1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
--                     999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [] 0) @?= [1000]
--   fst3 (eval 0 [9] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
--                     1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
--                     999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [] 0) @?= [1001]


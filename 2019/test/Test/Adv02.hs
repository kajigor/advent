module Test.Adv02 where 

import Test.HUnit (Assertion, (@?=))

import Adv02 (runProg, EvalRes (..), computeResult, findNounVerb) 

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output 

unit_runProg :: Assertion
unit_runProg = do
  test runProg [1,0,0,0,99] (Halt [2,0,0,0,99])
  test runProg [2,3,0,3,99] (Halt [2,3,0,6,99])
  test runProg [2,4,4,5,99,0] (Halt [2,4,4,5,99,9801]) 
  test runProg [1,1,1,4,99,5,6,0,99] (Halt [30,1,1,4,2,5,6,0,99])

unit_computeResult :: Assertion
unit_computeResult = do
  test computeResult [1,0,0,0,99] (Just 2)
  test computeResult [2,3,0,3,99] (Just 2)
  test computeResult [2,4,4,5,99,0] (Just 2) 
  test computeResult [1,1,1,4,99,5,6,0,99] (Just 30)

unit_findNounVerb :: Assertion
unit_findNounVerb = do
  test (findNounVerb 2) [1,0,0,0,99] (Just 0)
  test (findNounVerb 2) [2,0,0,3,99] (Just 0)
  test (findNounVerb 30) [1,0,0,4,99,5,6,0,99] (Just 0)
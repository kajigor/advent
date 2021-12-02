module Test.Adv16 where 

import Adv16 (vectorProd, pattern, phase, stage1)

import Test.HUnit (Assertion, (@?=))

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output  

unit_vectorProd = do 
  vectorProd [1,2,3,4,5,6,7,8] (pattern 1) @?= 4
  vectorProd [1,2,3,4,5,6,7,8] (pattern 2) @?= 8
  vectorProd [1,2,3,4,5,6,7,8] (pattern 3) @?= 2
  vectorProd [1,2,3,4,5,6,7,8] (pattern 4) @?= 2
  vectorProd [1,2,3,4,5,6,7,8] (pattern 5) @?= 6
  vectorProd [1,2,3,4,5,6,7,8] (pattern 6) @?= 1
  vectorProd [1,2,3,4,5,6,7,8] (pattern 7) @?= 5
  vectorProd [1,2,3,4,5,6,7,8] (pattern 8) @?= 8
  vectorProd [1,2,3,4,5,6,7,8] (pattern 9) @?= 0

unit_phase = do 
  test phase [1,2,3,4,5,6,7,8] [4,8,2,2,6,1,5,8]
  test phase [4,8,2,2,6,1,5,8] [3,4,0,4,0,4,3,8]
  test phase [3,4,0,4,0,4,3,8] [0,3,4,1,5,5,1,8]
  test phase [0,3,4,1,5,5,1,8] [0,1,0,2,9,4,9,8]

unit_stage1 =  do 
  test stage1 [8,0,8,7,1,2,2,4,5,8,5,9,1,4,5,4,6,6,1,9,0,8,3,2,1,8,6,4,5,5,9,5] [2,4,1,7,6,1,7,6]
  test stage1 [1,9,6,1,7,8,0,4,2,0,7,2,0,2,2,0,9,1,4,4,9,1,6,0,4,4,1,8,9,9,1,7] [7,3,7,4,5,4,1,8]
  test stage1 [6,9,3,1,7,1,6,3,4,9,2,9,4,8,6,0,6,3,3,5,9,9,5,9,2,4,3,1,9,8,7,3] [5,2,4,3,2,1,3,3]
  
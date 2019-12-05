module Test.Adv04 where 

import Adv04 
  ( numOfPasswords
  , adjacentDigits 
  , ascendingSequence
  , getDigits 
  , isPassword 
  , allPasswords
  , isPassword2
  , adjascentDigits2
  )

import Test.HUnit (Assertion, (@?=))

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output  

unit_adjacentDigits :: Assertion 
unit_adjacentDigits = do 
  test adjacentDigits [1,2,3,4,5,6] False 
  test adjacentDigits [1,2,3,4,1,6] False 
  test adjacentDigits [1,2,3,3,5,6] True 
  test adjacentDigits [1,1,3,4,5,6] True 
  test adjacentDigits [1,2,3,6,6,6] True 

unit_adjascentDigits2 :: Assertion 
unit_adjascentDigits2 = do 
  test adjascentDigits2 [1,2,3,4,5,6] False 
  test adjascentDigits2 [1,2,3,4,1,6] False 
  test adjascentDigits2 [1,2,3,6,6,6] False 
  test adjascentDigits2 [1,2,3,3,5,6] True 
  test adjascentDigits2 [1,1,3,4,5,6] True 
  test adjascentDigits2 [1,1,1,1,2,2] True

unit_ascendingSequence :: Assertion 
unit_ascendingSequence = do 
  test ascendingSequence [1,2,3,4,5,6] True 
  test ascendingSequence [1,2,3,6,6,6] True 
  test ascendingSequence [1,1,1,1,1,1] True 
  test ascendingSequence [1,8,3,4,5,6] False 
  test ascendingSequence [1,2,3,4,5,1] False 
  test ascendingSequence [1,8,8,8,5,6] False 

unit_getDigits :: Assertion 
unit_getDigits = do 
  test getDigits 123456 [1,2,3,4,5,6]
  test getDigits 123400 [1,2,3,4,0,0]
  
unit_isPassword :: Assertion 
unit_isPassword = do 
  test isPassword 111111 True 
  test isPassword 111123 True 
  test isPassword 122345 True 
  test isPassword 223450 False 
  test isPassword 123789 False 

unit_isPassword2 :: Assertion 
unit_isPassword2 = do 
  test isPassword2 111111 False 
  test isPassword2 111123 False 
  test isPassword2 122345 True 
  test isPassword2 223450 False 
  test isPassword2 123789 False 


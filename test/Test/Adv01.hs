module Test.Adv01 where 

import Test.HUnit (Assertion, (@?=))

import Adv01 (fuel, allFuel) 

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output 

unit_fuel :: Assertion
unit_fuel = do
  test fuel 12 2 
  test fuel 14 2 
  test fuel 1969 654
  test fuel 100756 33583 

unit_allFuel :: Assertion 
unit_allFuel = do 
  test allFuel 14 2 
  test allFuel 1969 966 
  test allFuel 100756 50346 



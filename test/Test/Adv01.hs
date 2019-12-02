module Test.Adv01 where 

import Test.HUnit (Assertion, (@?=))

import Adv01 (fuel, allFuel) 

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output 

unit_fuel0 :: Assertion
unit_fuel0 = test fuel 12 2 
    
unit_fuel1 :: Assertion
unit_fuel1 = test fuel 14 2 

unit_fuel2 :: Assertion
unit_fuel2 = test fuel 1969 654

unit_fuel3 :: Assertion
unit_fuel3 = test fuel 100756 33583 

unit_allFuel0 :: Assertion 
unit_allFuel0 = test allFuel 14 2 

unit_allFuel1 :: Assertion 
unit_allFuel1 = test allFuel 1969 966 

unit_allFuel2 :: Assertion 
unit_allFuel2 = test allFuel 100756 50346 



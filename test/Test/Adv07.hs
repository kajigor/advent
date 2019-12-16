module Test.Adv07 where 

import Test.HUnit (Assertion, (@?=))
import Data.Tree (Tree (..))
import Adv07 (findBiggestOutput2)

  
test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output  

-- unit_part2 = do 
--   test (\prog -> findBiggestOutput2 prog 0 [5 .. 9]) (map show [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]) 139629729
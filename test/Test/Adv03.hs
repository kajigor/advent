module Test.Adv03 where 

import Test.HUnit (Assertion, (@?=))

import Adv03 (parseList, Instr (..), Coord (..), eval, coords, solve) 

import Data.List (sort)

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output 

test' :: (Eq c, Show c) => (a -> b -> c) -> a -> b -> c -> Assertion
test' f x y output = f x y @?= output 

unit_parseList :: Assertion 
unit_parseList = do 
  test parseList "U1,D12,L13,R90" [U 1, D 12, L 13, R 90]

unit_eval :: Assertion 
unit_eval = do 
  test (eval (Coord 3 (-4))) (R 3) [Coord 3 (-1), Coord 3 (-2), Coord 3 (-3), Coord 3 (-4)]

unit_coords :: Assertion
unit_coords = do 
  test (sort . coords) 
       [U 1, R 2, D 1, L 2] 
       (sort [Coord 0 0, Coord 1 0, Coord 1 1, Coord 1 2, Coord 0 2, Coord 0 1, Coord 0 0])

unit_solve :: Assertion
unit_solve = do 
  test' solve "R8,U5,L5,D3" "U7,R6,D4,L4" 6
  test' solve "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" 159
  test' solve "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" 135
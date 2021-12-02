module D02 where

data Dir = Forward Int | Up Int | Down Int

instance Show Dir where
  show (Forward n) = "forward " ++ show n
  show (Up n) = "up " ++ show n
  show (Down n) = "down " ++ show n

instance Read Dir where
  readsPrec _ xs =
    let [dir, n'] = words xs in
    let n = read n' in
    case dir of
      "forward" -> [(Forward n, "")]
      "up" -> [(Up n, "")]
      "down" -> [(Down n, "")]
      _ -> []

type Coord = (Int, Int)

move :: Coord -> Dir -> Coord
move (x, d) (Forward n) = (x + n, d)
move (x, d) (Up n) = (x, d - n)
move (x, d) (Down n) = (x, d + n)

task1 :: String -> Int
task1 xs =
  let ls = lines xs in
  let moves = map read ls in
  (\(x, d) -> x * d) $ foldl move (0, 0) moves

data State = State Aim Coord
type Aim = Int

task2 :: String -> Int
task2 xs =
  let ls = lines xs in
  let moves = map read ls in
  (\(State _ (x, d)) -> x * d) $ foldl stir (State 0 (0, 0)) moves

stir :: State -> Dir -> State
stir (State aim (x, d)) (Down n) = State (aim + n) (x, d)
stir (State aim (x, d)) (Up n) = State (aim - n) (x, d)
stir (State aim (x, d)) (Forward n) = State aim (x + n, d + aim * n)

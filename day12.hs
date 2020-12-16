{-# OPTIONS_GHC -Wall #-}
import Control.Arrow ((***))
import Data.Char (isDigit)

main :: IO ()
main = do
  input <- fmap (fmap parse . lines) getContents
  -- part 1
  print . (\(x, y, _) -> abs x + abs y) . foldl move1 (0, 0, 90) $ input
  -- part 2
  print . (\(x, y, _, _) -> abs x + abs y) . foldl move2 (0, 0, 10, 1) $ input
  return ()

data Command = N | S | E | W | L | R | F deriving (Read, Show)

parse :: String -> (Command, Int)
parse = (read *** read) . break isDigit

-- (x, y, angle)
move1 :: (Int, Int, Int) -> (Command, Int) -> (Int, Int, Int)
move1 (x, y, angle) (N, v) = (x, y + v, angle)
move1 (x, y, angle) (S, v) = (x, y - v, angle)
move1 (x, y, angle) (E, h) = (x + h, y, angle)
move1 (x, y, angle) (W, h) = (x - h, y, angle)
move1 (x, y, angle) (L, d) = (x, y, (angle - d) `mod` 360)
move1 (x, y, angle) (R, d) = (x, y, (angle + d) `mod` 360)
move1 (x, y, 0  ) (F, steps) = (x, y + steps, 0)
move1 (x, y, 90 ) (F, steps) = (x + steps, y, 90)
move1 (x, y, 180) (F, steps) = (x, y - steps, 180)
move1 (x, y, 270) (F, steps) = (x - steps, y, 270)
move1 (_, _, _) (F, _) = error "bad angle"

-- (x, y, wayX, wayY)
move2 :: (Int, Int, Int, Int) -> (Command, Int) -> (Int, Int, Int, Int)
move2 (x, y, wayX, wayY) (N, s) = (x, y, wayX, wayY + s)
move2 (x, y, wayX, wayY) (S, s) = (x, y, wayX, wayY - s)
move2 (x, y, wayX, wayY) (E, s) = (x, y, wayX + s, wayY)
move2 (x, y, wayX, wayY) (W, s) = (x, y, wayX - s, wayY)
move2 (x, y, wayX, wayY) (L, 0) = (x, y, wayX, wayY)
move2 (x, y, wayX, wayY) (R, 0) = (x, y, wayX, wayY)
move2 (x, y, wayX, wayY) (L, s) = move2 (x, y, -wayY, wayX) (L, s - 90)
move2 (x, y, wayX, wayY) (R, s) = move2 (x, y, wayY, -wayX) (R, s - 90)
move2 (x, y, wayX, wayY) (F, s) = (x + s * wayX, y + s * wayY, wayX, wayY)

{-# OPTIONS_GHC -Wall #-}
import Data.List (sort)
import Control.Applicative (liftA2)

main :: IO ()
main = do
  rawInput <- fmap ((0:) . sort . fmap (read :: String -> Int) . lines) getContents
  -- part 1
  print . part1 $ rawInput
  -- part 2
  print . part2 $ rawInput

part1 :: [Int] -> Int
part1 xs = liftA2 (*) (count 1) (succ . count 3) . fmap (\(x, y) -> y - x) $ zip xs (tail xs)

part2 :: [Int] -> Int
part2 [] = error "should not happen"
part2 [_] = 1
part2 [x, y]
  | y - x > 3 = 0
  | otherwise = 1
part2 (x:y:_)
  | y - x > 3 = 0
part2 (a:b:c:d:xs)
  | d - a <= 3 = let route1 = part2 (b:c:d:xs)
                     route2 = part2 (c:d:xs)
                     route3 = part2 (d:xs)
                 in route1 + route2 + route3
  | c - a <= 3 = let route1 = part2 (b:c:d:xs)
                     route2 = part2 (c:d:xs)
                 in route1 + route2
  | otherwise = part2 (b:c:d:xs)
part2 (x:y:z:xs)
  | z - x <= 3 = let route1 = part2 (y:z:xs)
                     route2 = part2 (z:xs)
                 in route1 + route2
  | otherwise = part2 (y:z:xs)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

{-# OPTIONS_GHC -Wall #-}
import Data.List (sort)
import Control.Applicative (liftA2)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = do
  rawInput <- fmap ((0:) . sort . fmap (read :: String -> Int) . lines) getContents
  -- part 1
  print . part1 $ rawInput
  -- part 2
  print . snd . flip part2 M.empty $ rawInput

part1 :: [Int] -> Int
part1 xs = liftA2 (*) (count 1) (succ . count 3) . fmap (\(x, y) -> y - x) $ zip xs (tail xs)

part2 :: [Int] -> M.Map Int Int -> (M.Map Int Int, Int)
part2 [] _ = error "should not happen"
part2 [_] m = (m, 1)
part2 [x, y] m
  | y - x > 3 = (m, 0)
  | otherwise = (m, 1)
part2 (x:y:_) m
  | y - x > 3 = (m, 0)
part2 (x:_) m
  | isJust $ M.lookup x m = (m, fromJust $ M.lookup x m)
part2 (a:b:c:d:xs) m
  | d - a <= 3 = let (m', route1) = part2 (b:c:d:xs) m
                     (m'', route2) = part2  (c:d:xs) m'
                     (m''', route3) = part2 (d:xs) m''
                     newCount = route1 + route2 + route3
                 in (M.insert a newCount m''', route1 + route2 + route3)
  | c - a <= 3 = let (m', route1) = part2 (b:c:d:xs) m
                     (m'', route2) = part2 (c:d:xs) m'
                     newCount = route1 + route2
                 in (M.insert a newCount m'', newCount)
  | otherwise = part2 (b:c:d:xs) m
part2 (x:y:z:xs) m
  | z - x <= 3 = let (m', route1) = part2 (y:z:xs) m
                     (m'', route2) = part2 (z:xs) m'
                     newCount = route1 + route2
                 in (M.insert x newCount m'', newCount)
  | otherwise = part2 (y:z:xs) m

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

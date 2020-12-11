{-# OPTIONS_GHC -Wall #-}
import Data.List (tails, sort)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  rawInput <- fmap (fmap read . lines) getContents
  -- part 1
  let invalid = part1 . reverse $ rawInput
  print invalid
  -- part 2
  print . (\xs -> head xs + last xs) . sort . part2 invalid . reverse . tails $ rawInput


part1 :: [Int] -> Int
part1 (a:xs) = let smaller = take 25 xs in
               if null [(x, y) | x <- smaller, y <- smaller, x /= y, x + y == a] then a else part1 xs
part1 [] = error "should not happen"

part2 :: Int -> [[Int]] -> [Int]
part2 target = head . filter ((/= 1) . length) . catMaybes . fmap (check target)
  where
  check :: Int -> [Int] -> Maybe [Int]
  check 0 _ = Just []
  check _ [] = Nothing
  check n (x:xs)
    | x > n = Nothing
    | otherwise = case check (n - x) xs of
      Nothing -> Nothing
      Just rest -> Just (x:rest)

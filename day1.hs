{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
  input <- fmap (fmap read . lines) getContents
  -- part 1
  print $ part1 input
  -- part 2
  print $ part2 input

part1 :: [Int] -> Int
part1 list = uncurry (*) . head . filter (\(a,b) -> a + b == 2020) $ cartProd2 list list
  where
  cartProd2 xs ys = [(x, y) | x <- xs, y <- ys]

part2 :: [Int] -> Int
part2 list = (\(a, b, c) -> a * b *c) . head . filter (\(a,b,c) -> a + b + c == 2020) $ cartProd3 list list list
  where
  cartProd3 xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

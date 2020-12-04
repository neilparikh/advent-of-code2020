{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
  treeMap <- fmap lines getContents
  let width = length . head $ treeMap
  let height = length treeMap
  -- Part 1
  print $ walk treeMap (0, 0) (width, height) (3, 1)
  -- Part 2
  print . product . fmap (walk treeMap (0, 0) (width, height)) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  return ()

walk :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
walk treeMap (x, y) (width, height) (xStep, yStep)
  | y >= height = 0
  | x >= width = walk treeMap (x `mod` width, y) (width, height) (xStep, yStep)
  | ((treeMap !! y) !! x) == '#' = 1 + walk treeMap (x + xStep, y + yStep) (width, height) (xStep, yStep)
  | otherwise = walk treeMap (x + xStep, y + yStep) (width, height) (xStep, yStep)

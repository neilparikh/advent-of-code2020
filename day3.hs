{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Monad.Reader

main :: IO ()
main = do
  treeMap <- fmap lines getContents
  let width = length . head $ treeMap
  let height = length treeMap
  -- Part 1
  print $ runReader walk (treeMap, (width, height), (3, 1))
  -- Part 2
  print . product . fmap (\step -> runReader walk (treeMap, (width, height), step)) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  return ()

walk :: Reader ([String], (Int, Int), (Int, Int)) Int
walk = walk' (0, 0)

walk' :: (Int, Int) -> Reader ([String], (Int, Int), (Int, Int)) Int
walk' (x, y) = do
  (treeMap, (width, height), (xStep, yStep)) <- ask
  if | y >= height -> return 0
     | x >= width -> walk' (x `mod` width, y)
     | ((treeMap !! y) !! x) == '#' -> (+ 1) <$> walk' (x + xStep, y + yStep)
     | otherwise -> walk' (x + xStep, y + yStep)

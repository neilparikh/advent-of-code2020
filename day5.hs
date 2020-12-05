{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (sort)

main :: IO ()
main = do
  input <- fmap lines getContents
  let ids = fmap (calcId . parse) input
  -- part 1
  print . maximum $ ids
  -- part 2
  print . missing . sort $ ids
  return ()

missing :: [Int] -> Int
missing [] = error "at end of list"
missing [_] = error "at end of list"
missing (x:y:zs)
  | x + 1 == y = missing (y:zs)
  | otherwise = x + 1

calcId :: (Int, Int) -> Int
calcId (r, c) = r * 8 + c

parse :: String -> (Int, Int)
parse (splitAt 7 -> (rowC, colC)) = (parseRow rowC, parseCol colC)

parseRow :: String -> Int
parseRow = go (0, 127)
  where
  go (l, _) [] = l
  go (l, h) ('F':xs) = go (l, h - (h - l + 1)  `div`  2) xs
  go (l, h) ('B':xs) = go (l + (h - l + 1)  `div`  2, h) xs
  go (_, _) (_:_) = error "bad input"

parseCol :: String -> Int
parseCol = go (0, 7)
  where
  go (l, _) [] = l
  go (l, h) ('L':xs) = go (l, h - (h - l + 1)  `div`  2) xs
  go (l, h) ('R':xs) = go (l + (h - l + 1)  `div`  2, h) xs
  go (_, _) (_:_) = error "bad input"

{-# OPTIONS_GHC -Wall #-}
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  rawInput <- getContents
  let input = wordsBy (== "") . lines $ rawInput
  -- part 1
  print . sum . fmap (S.size . S.fromList . concat) $ input
  -- part 2
  print . sum . fmap (S.size . intersection . fmap S.fromList) $ input
  return ()

intersection :: Ord a => [Set a] -> Set a
intersection [] = S.empty
intersection (x:xs) = foldr S.intersection x xs

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f lst = go lst []
  where
  go [] acc = reverse (fmap reverse acc)
  go (x:xs) []
    | f x = go xs [[]]
    | otherwise = go xs [[x]]
  go (x:xs) (a:as)
    | f x = go xs ([]:a:as)
    | otherwise = go xs ((x:a):as)

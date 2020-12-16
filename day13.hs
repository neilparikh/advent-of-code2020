{-# OPTIONS_GHC -Wall #-}
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&), second)

main :: IO ()
main = do
  time <- fmap read getLine
  rawIds <- wordsBy (== ',') <$> getLine
  -- part 1
  let ids1 = fmap read . filter (/= "x") $ rawIds
  print . uncurry (*) . minimumBy (comparing snd) . fmapWithTag (timeToBus time) $ ids1
  -- part 2
  let ids2 = fmap (second (read :: String -> Int)) . filter ((/= "x") . snd) . zip [0..] $ rawIds
  print ids2
  return ()

timeToBus :: Int -> Int -> Int
timeToBus time bus = bus - (time `mod` bus)

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

fmapWithTag :: Functor f => (a -> b) -> f a -> f (a, b)
fmapWithTag f = fmap (id &&& f)

{-# OPTIONS_GHC -Wall #-}
import Data.List (minimumBy, foldl1')
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
  let ids2 = fmap ((\(a, n) -> (n - a, n)) . second read) . filter ((/= "x") . snd) . zip [0..] $ rawIds
  print . uncurry mod . foldl1' solve2 $ ids2

timeToBus :: Integer -> Integer -> Integer
timeToBus time bus = bus - (time `mod` bus)

solve2 :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solve2 (a1, n1) (a2, n2) = let (m1, m2) = egcd n1 n2
                               n' = n1 * n2
                               a' = (a1 * m2 * n2 + a2 * m1 * n1) `mod` n'
                           in (a', n')

egcd :: Integer -> Integer -> (Integer, Integer)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
  (q, r) = quotRem a b
  (s, t) = egcd b r

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

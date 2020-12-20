{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Strict #-}

import qualified Data.IntMap.Strict as IM
import Debug.Trace (trace)

data Times = Once Int | More Int Int deriving Show

main :: IO ()
main = do
  input <- fmap read . wordsBy (== ',') <$> getLine
  let initialHistory = IM.fromList $ zip input (fmap Once [1..])
  -- part 1
  print $ runUntil 2020 initialHistory (length input + 1) (last input)
  -- part 2
  -- FIXME: this is slow (~2 mins)
  print $ runUntil 30000000 initialHistory (length input + 1) (last input)

runUntil :: Int -> IM.IntMap Times -> Int -> Int -> Int
runUntil _ _ i _ | (i `mod` 1000000 == 0) && trace (show i) False = error ""
runUntil target history turn prev
  | turn == target = fst $ next prev turn history
  | otherwise = let (ans, history') = next prev turn history in runUntil target history' (succ turn) ans

next :: Int -> Int -> IM.IntMap Times -> (Int, IM.IntMap Times)
next prev turn history = case IM.lookup prev history of
  Nothing -> error "should not happen"
  Just (Once _) -> (0, IM.insertWith timesUpdate 0 (Once turn) history)
  Just (More y x) -> (y - x, IM.insertWith timesUpdate (y - x) (Once turn) history)

timesUpdate :: Times -> Times -> Times
timesUpdate (Once y) (Once x)= More y x
timesUpdate (Once y) (More x _) = More y x
timesUpdate (More _ _) _ = error "should not happen"

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

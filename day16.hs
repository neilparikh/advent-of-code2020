{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (isRight, rights)
import Control.Monad (unless)
import Data.List (transpose, isPrefixOf)
import qualified Data.Set as S

import Parser

type Range = (Int, Int)

main :: IO ()
main = do
  [rules', my', other'] <- (wordsBy (== "") . lines) <$> getContents
  let my = fmap (read :: String -> Int) . wordsBy (== ',') $ my' !! 1
  let other = fmap (read :: String -> Int) . wordsBy (== ',') <$> tail other'
  let parsedRules' = fmap (applyParser ruleParser) rules'
  unless (all isRight parsedRules') (error "parse error")
  let rules = rights parsedRules'
  -- part 1
  print . sum . concatMap (invalidFields (fmap snd rules)) $ other
  -- part 2
  let validOther = filter (null . invalidFields (fmap snd rules)) other
  let possibleNames = fmap (fmap (`possibleFields` rules)) validOther
  let names = fmap (head . S.toList) . narrow . fmap intersection . transpose $ possibleNames
  print . product . fmap snd . filter (isPrefixOf "departure" . fst) $ zip names my
  return ()

narrow :: [S.Set String] -> [S.Set String]
narrow xs | all ((== 1) . S.size) xs = xs
narrow xs = let minimal = filter ((== 1) . S.size) xs
                el = fmap (head . S.toList) minimal
            in narrow $ fmap (\x -> if S.size x /= 1 then foldl (flip S.delete) x el else x) xs

possibleFields :: Int -> [(String, (Range, Range))] -> S.Set String
possibleFields x = S.fromList . fmap fst . filter (check x . snd)

invalidFields :: [(Range, Range)] -> [Int] -> [Int]
invalidFields rules = filter invalid
  where
  invalid x = not $ any (check x) rules

check :: Int -> (Range, Range) -> Bool
check x ((a, b), (c, d)) = (x >= a && x <= b) || (x >= c && x <= d)

ruleParser :: Parser (String, (Range, Range))
ruleParser = do
  name <- many1 (letter <|> space)
  _ <- string ": "
  a <- read <$> many1 digit
  _ <- char '-'
  b <- read <$> many1 digit
  _ <- string " or "
  c <- read <$> many1 digit
  _ <- char '-'
  d <- read <$> many1 digit
  return (name, ((a, b), (c, d)))

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

intersection :: Ord a => [S.Set a] -> S.Set a
intersection [] = S.empty
intersection (x:xs) = foldr S.intersection x xs

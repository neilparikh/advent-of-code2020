{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (isRight, rights)
import Control.Monad (unless)
import Parser

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
  return ()

type Range = (Int, Int)

invalidFields :: [(Range, Range)] -> [Int] -> [Int]
invalidFields rules = filter invalid
  where
  invalid x = not $ any (check x) rules
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

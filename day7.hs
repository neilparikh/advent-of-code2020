{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (rights)
import Data.List (lookup)

import Parser

main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let bagMap = rights . fmap (applyParser ruleParser) $ rawInput
  -- part 1
  print . length . filter (isIn bagMap ("shiny", "gold")) $ fmap fst bagMap
  -- part 2
  print $ numBags bagMap ("shiny", "gold")
  return ()

isIn :: [Rule] -> Bag -> Bag -> Bool
isIn bagMap child parent = case lookup parent bagMap of
  Just xs -> child `elem` fmap fst xs || any (isIn bagMap child) (fmap fst xs)
  Nothing -> False

numBags :: [Rule] -> Bag -> Int
numBags bagMap bag = case lookup bag bagMap of
  Just xs -> sum $ fmap countForBag xs
  Nothing -> 0
  where
  countForBag (bag', n) = n * (1 + numBags bagMap bag')

type Bag = (String, String)
type Rule = (Bag, [(Bag, Int)])

ruleParser :: Parser Rule
ruleParser = do
  rootBag <- bagParser
  _ <- string " contain "
  otherBags <- constString "no other bags" [] <|> sepBy bagWithCountParser (string ", ")
  _ <- char '.'
  return (rootBag, otherBags)

bagWithCountParser :: Parser (Bag, Int)
bagWithCountParser = do
  num <- many1 digit
  _ <- char ' '
  bag <- bagParser
  return (bag, read num)

bagParser :: Parser Bag
bagParser = do
  word1 <- many1 letter
  _ <- char ' '
  word2 <- many1 letter
  _ <- char ' '
  _ <- string "bag"
  _ <- optional (char 's')
  return (word1, word2)

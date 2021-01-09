{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (isRight, rights)
import Data.Maybe (fromJust)
import Control.Monad (unless, join, void)
import qualified Data.IntMap as IM

import Text.Regex.PCRE

import Parser

printUtil :: Show a => [a] -> IO ()
printUtil = putStrLn . unlines . fmap show

main :: IO ()
main = do
  [rules', input] <- (wordsBy (== "") . lines) <$> getContents
  let parsedRules' = fmap (applyParser declParser) rules'
  unless (all isRight parsedRules') (error "parse error\n")
  let rules = IM.fromList $ rights parsedRules'
  -- part 1
  let finalRule = deRef rules (fromJust (IM.lookup 0 rules))
  let regex = "^" ++ compile finalRule ++ "$"
  print . length . filter (=~ regex) $ input
  -- part 2
  let parserMap = fmap (toParser . deRef rules) rules
  let parser0p1 = fromJust (IM.lookup 0 parserMap) -- part1 again with parser approach
  print . length . rights . fmap (applyParser $ parser0p1 >> eofParser) $ input
  return ()

eofParser :: Parser ()
eofParser = do
  xs <- many anyChar
  if null xs then return () else fail "leftover"

toParser :: Rule -> Parser ()
toParser (Lit x) = void $ string x
toParser (Or a b) = try (toParser a) <|> toParser b
toParser (Seq xs) = sequence_ (fmap toParser xs)
toParser (Ref _) = error "should not happen"

compile :: Rule -> String
compile (Seq xs) = join $ fmap compile xs
compile (Lit x) = x
compile (Or a b) = "(" ++ compile a ++ "|" ++ compile b ++ ")"
compile (Ref _) = error "should not happen"

deRef :: IM.IntMap Rule -> Rule -> Rule
deRef _ (Lit x) = Lit x
deRef m (Or x y) = Or (deRef m x) (deRef m y)
deRef m (Seq xs) = Seq $ fmap (deRef m) xs
deRef m (Ref i) = case IM.lookup i m of
  Just r -> deRef m r
  Nothing -> error $ "can't find rule " ++ show i

data Rule = Lit String | Ref Int | Seq [Rule] | Or Rule Rule
  deriving Show

declParser :: Parser (Int, Rule)
declParser = do
  n <- read <$> many1 digit
  _ <- string ": "
  rule <- ruleParser
  return (n, rule)

ruleParser :: Parser Rule
ruleParser = try orParser <|> seqParser <|> litParser

orParser :: Parser Rule
orParser = do
  rule1 <- seqParser <|> litParser
  _ <- wrapWithSpaces (string "|")
  rule2 <- seqParser <|> litParser
  return $ Or rule1 rule2

seqParser :: Parser Rule
seqParser = do
  rules <- many1 (many1 digit <* optional (string " "))
  return . Seq $ fmap (Ref . read) rules

litParser :: Parser Rule
litParser = Lit . (:[]) <$> wrapWith (char '"') letter

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

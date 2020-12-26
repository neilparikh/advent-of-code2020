{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (isRight, rights)
import Control.Monad (unless)
import Debug.Trace (traceM)

import Parser

main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let parsedInput' = fmap (applyParser expParser) rawInput
  unless (all isRight parsedInput') (error "parse error\n")
  let parsedInput = rights parsedInput'
  print . sum $ parsedInput

expParser :: Parser Int
expParser = do
  a <- numberParser <|> parens expParser
  rest <- many1 restParser
  return $ foldl (\e f -> f e) a rest

restParser :: Parser (Int -> Int)
restParser = do
  op <- wrapWithSpaces (constString "*" (*) <|> constString "+" (+))
  b <- parens expParser <|> numberParser
  return $ flip op b

numberParser :: Parser Int
numberParser = read <$> many1 digit

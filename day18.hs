{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (isRight, rights)
import Control.Monad (unless)
import Data.Char (isDigit)

import Parser

main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let parsedInput' = fmap (applyParser expParser) rawInput
  unless (all isRight parsedInput') (error "parse error\n")
  let parsedInput = rights parsedInput'
  print . sum $ parsedInput
  print . sum . fmap (evalRPN [] [] . shunting [] [] . tokenize) $ rawInput

evalRPN :: [String] -> [Int] -> [String] -> Int
evalRPN [] [x] [] = x
evalRPN ("+":ops) (a:b:nums) xs = evalRPN ops ((a + b):nums) xs
evalRPN ("*":ops) (a:b:nums) xs = evalRPN ops ((a * b):nums) xs
evalRPN ops nums (x:xs)
  | all isDigit x = evalRPN ops (read x :nums) xs
evalRPN ops nums ("+":xs) = evalRPN ("+":ops) nums xs
evalRPN ops nums ("*":xs) = evalRPN ("*":ops) nums xs

shunting :: [String] -> [String] -> [String] -> [String]
shunting ops out [] = reverse $ reverse ops ++ out
shunting ops@("+":_) out ("*":xs) = let (a, b) = break (/= "+") ops
                                    in shunting ("*":b) (a ++ out) xs
shunting ops out ("*":xs) = shunting ("*":ops) out xs
shunting ops out ("+":xs) = shunting ("+":ops) out xs
shunting ops out ("(":xs) = shunting ("(":ops) out xs
shunting ops out (")":xs) = let (a, b) = break (== "(") ops
                            in shunting (tail b) (reverse a ++ out) xs
shunting ops out (x:xs)
  | all isDigit x = shunting ops (x:out) xs

tokenize :: String -> [String]
tokenize = filter (not . null) . concatMap splitCloseParen . concatMap splitOpenParen . words
  where
  splitOpenParen :: String -> [String]
  splitOpenParen = (\(a, b) -> fmap (:[]) a ++ [b]) . break (/= '(')
  splitCloseParen :: String -> [String]
  splitCloseParen = (\(a, b) -> a : fmap (:[]) b) . break (== ')')

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

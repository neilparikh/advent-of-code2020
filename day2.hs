{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (isRight, rights)
import Control.Monad (unless)

import Parser

main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let parsedInput' = fmap (applyParser entryParser) rawInput
  unless (all isRight parsedInput') (error "parse error")
  let parsedInput = rights parsedInput'
  -- Part 1
  print . length . filter isValid1 $ parsedInput
  -- Part 2
  print . length . filter isValid2 $ parsedInput

data Policy = Policy Int Int Char

isValid2 :: (Policy, String) -> Bool
isValid2 (Policy i j c, password) = (password !! (i - 1) == c) `xor` (password !! (j - 1) == c)
  where
  xor a b = (a && not b) || (b && not a)

isValid1 :: (Policy, String) -> Bool
isValid1 (Policy lowerBound upperBound c, password) = n >= lowerBound && n <= upperBound
  where
  n = length . filter (== c) $ password

entryParser :: Parser (Policy, String)
entryParser = do
  policy <- policyParser
  _ <- string ": "
  password <- many1 letter
  return (policy, password)

policyParser :: Parser Policy
policyParser = do
  i <- fmap read (many1 digit)
  _ <- char '-'
  j <- fmap read (many1 digit)
  _ <- char ' '
  c <- letter
  return $ Policy i j c

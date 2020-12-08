{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (rights)
import Data.Maybe (isJust)

import Parser

main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let program = rights . fmap (applyParser insnParser) $ rawInput
  -- part 1
  print $ runTillLoop (0, 0) program []
  -- part 2
  let flippedPrograms = flipProgram program
  print . head . filter isJust . fmap (\p -> terminates (0, 0) p []) $ flippedPrograms

runTillLoop :: ProgramState -> [(Insn, Int)] -> [Int] -> Int
runTillLoop (ip, acc) program seen
  | ip `elem` seen = acc
  | otherwise = let s = step (ip, acc) (program !! ip)
                in runTillLoop s program (ip:seen)

flipProgram :: [(Insn, Int)] -> [[(Insn, Int)]]
flipProgram [] = []
flipProgram (x@(Acc, _):xs) = fmap (x:) (flipProgram xs)
flipProgram (x@(Nop, n):xs) = ((Jmp, n):xs): fmap (x:) (flipProgram xs)
flipProgram (x@(Jmp, n):xs) = ((Nop, n):xs): fmap (x:) (flipProgram xs)

terminates :: ProgramState -> [(Insn, Int)] -> [Int] -> Maybe Int
terminates (ip, acc) program seen
  | ip == length program = Just acc
  | ip `elem` seen = Nothing
  | otherwise = let s = step (ip, acc) (program !! ip)
                in terminates s program (ip:seen)

step :: ProgramState -> (Insn, Int) -> ProgramState
step (ip, acc) (Nop, _) = (ip + 1, acc)
step (ip, acc) (Acc, n) = (ip + 1, acc + n)
step (ip, acc) (Jmp, n) = (ip + n, acc)

data Insn = Acc | Jmp | Nop deriving Show
type ProgramState = (Int, Int) -- (insn pointer, acc)

insnParser :: Parser (Insn, Int)
insnParser = do
  insn <- constString "acc" Acc <|> constString "jmp" Jmp <|> constString "nop" Nop
  _ <- char ' '
  n <- intParser
  return (insn, n)

intParser :: Parser Int
intParser = do
  sign <- constString "+" ' ' <|> char '-'
  num <- many1 digit
  return . read $ sign:num

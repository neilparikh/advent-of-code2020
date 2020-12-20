{-# OPTIONS_GHC -Wall #-}
import Text.Parsec
import Data.Either (isRight, rights)
import Control.Monad (unless)
import qualified Data.IntMap.Strict as IM
import Data.Bits ((.&.), (.|.), setBit)
import Data.List (foldl')

import Parser

data Program = Mask String
             | Assign Int Int
             deriving Show

data ProgramState = PS { storedMask :: String, mem :: IM.IntMap Int }
                  deriving Show

main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let parsedInput' = fmap (applyParser programParser) rawInput
  unless (all isRight parsedInput') (error "parse error")
  let parsedInput = rights parsedInput'
  -- part 1
  print . sum . IM.elems . mem . foldl' exec1 (PS "" IM.empty) $ parsedInput
  -- part 2
  print . sum . IM.elems . mem . foldl' exec2 (PS "" IM.empty) $ parsedInput

exec2 :: ProgramState -> Program -> ProgramState
exec2 (PS _ m) (Mask mask') = PS mask' m
exec2 (PS mask m) (Assign addr val) = PS mask m

exec1 :: ProgramState -> Program -> ProgramState
exec1 (PS _ m) (Mask mask') = PS mask' m
exec1 (PS mask m) (Assign addr val) = PS mask $ IM.insert addr (computeMask mask val) m
  where
  -- 0 mask = X -> 1
  -- 1 mask = X -> 0
  -- ans = (x & 0 mask) | 1 mask
  computeMask mask x = (x .|. oneMask mask 35) .&. zeroMask mask 35
  zeroMask "" _  = 0
  zeroMask ('X':xs) i = setBit (zeroMask xs (pred i)) i
  zeroMask ('1':xs) i = setBit (zeroMask xs (pred i)) i
  zeroMask ('0':xs) i = zeroMask xs (pred i)
  zeroMask _ _ = error "bad mask"
  oneMask "" _  = 0
  oneMask ('X':xs) i = oneMask xs (pred i)
  oneMask ('1':xs) i = setBit (oneMask xs (pred i)) i
  oneMask ('0':xs) i = oneMask xs (pred i)
  oneMask _ _ = error "bad mask"

programParser :: Parser Program
programParser = try maskParser <|> assignParser

maskParser :: Parser Program
maskParser = do
  _ <- string "mask = "
  mask <- many1 (char 'X' <|> char '1' <|> char '0')
  return $ Mask mask

assignParser :: Parser Program
assignParser = do
  _ <- string "mem["
  i <- read <$> many1 digit
  _ <- string "] = "
  val <- read <$> many1 digit
  return $ Assign i val

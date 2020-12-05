{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
import Data.List (sortOn)
import Data.Char (isDigit, isAlpha)

main :: IO ()
main = do
  rawInput <- getContents
  let input = fmap (sortOn fst . parse) . wordsBy (== "") . lines $ rawInput
  -- part 1
  print . length . filter isValid1 $ input
  -- part 2
  print . length . filter isValid2 $ input
  return ()

isValid1 :: [(String, String)] -> Bool
isValid1 (fmap fst -> x) = x == fmap fst validators

isValid2 :: [(String, String)] -> Bool
isValid2 x = isValid1 x && and (zipWith ($) (fmap snd validators) (fmap snd x))

validators :: [(String, String -> Bool)]
validators = sortOn fst [
  ("byr", yrValid 1920 2002),
  ("iyr", yrValid 2010 2020),
  ("eyr", yrValid 2020 2030),
  ("hgt", hgtValid),
  ("hcl", hclValid),
  ("ecl", eclValid),
  ("pid", pidValid)
  ]

yrValid :: Int -> Int -> String -> Bool
yrValid lo hi yr = all isDigit yr && let y = read yr in inRange lo hi y

hgtValid :: String -> Bool
hgtValid hgt = let (n, unit) = break isAlpha hgt
               in not (null n || any (not . isDigit) n) && (case unit of
                         "cm" -> inRange 150 193 (read n)
                         "in" -> inRange 59 76 (read n)
                         _ -> False)

hclValid :: String -> Bool
hclValid ('#':lst@[_, _, _, _, _, _]) = all (`elem` (['0'..'9'] ++ ['a'..'f'])) lst
hclValid _ = False

eclValid :: String -> Bool
eclValid = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pidValid :: String -> Bool
pidValid pid = length pid == 9 && all isDigit pid

parse :: [String] -> [(String, String)]
parse = filter ((/= "cid") . fst) . foldr (\x -> (++) (fmap helper . wordsBy (== ' ') $ x)) []
  where
  helper = (\[a, b] -> (a, b)) . wordsBy (== ':')

inRange :: Int -> Int -> Int -> Bool
inRange lo hi v = lo <= v && v <= hi

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


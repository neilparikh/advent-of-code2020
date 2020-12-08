module Parser where
import Text.Parsec

type Parser a = Parsec String () a

applyParser :: Parser a -> String -> Either ParseError a
applyParser parser = runParser parser () ""

constString :: String -> a -> Parser a
constString s f = string s *> return f

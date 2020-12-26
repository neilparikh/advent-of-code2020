module Parser where
import Text.Parsec

type Parser a = Parsec String () a

applyParser :: Parser a -> String -> Either ParseError a
applyParser parser = runParser parser () ""

constString :: String -> a -> Parser a
constString s f = string s *> return f

wrapWith :: Parser a -> Parser b -> Parser b
wrapWith wrapper subParser = wrapper *> subParser <* wrapper

wrapWithSpaces :: Parser a -> Parser a
wrapWithSpaces = wrapWith spaces

parens :: Parser a -> Parser a
parens subParser = char '(' *> subParser <* char ')'

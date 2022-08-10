module Parser where

import Relude
import qualified Relude.Unsafe as Unsafe

newtype Parser a = Parser {getParser :: StateT String Maybe a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      Alternative
    )

runParse :: Parser a -> String -> Maybe (a, String)
runParse = runStateT . getParser

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ do
  (c : cs) <- get
  guard $ pred c
  put cs
  return c

char :: Char -> Parser Char
char x = satisfy (== x)

eof :: Parser ()
eof = Parser $ do
  "" <- get
  return ()

string :: String -> Parser String
string [] = pure []
string (x : xs) = do
  char x
  string xs
  pure (x : xs)

oneOf :: [Parser a] -> Parser a
oneOf = asum

oneOfChar :: String -> Parser Char
oneOfString :: [String] -> Parser String
oneOfChar = oneOf . fmap char

oneOfString = oneOf . fmap string

alphabetic :: Parser Char
alphabetic = oneOfChar ['A' .. 'Z'] <|> oneOfChar ['a' .. 'z']

zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne = optional

digit, leadingDigit :: Parser Char
digit = oneOfChar ['0' .. '9']
leadingDigit = oneOfChar ['1' .. '9']

natural :: Parser Integer
natural =
  Unsafe.read <$> do
    lead <- leadingDigit
    rest <- many digit
    pure $ lead : rest

zero :: Parser Integer
zero = char '0' $> 0

integer :: Parser Integer
integer =
  zero <|> do
    minus <- zeroOrOne (char '-')
    num <- natural
    return $ fromMaybe num (negate num <$ minus)

whitespace :: Parser Char
whitespace = oneOfChar " \t\r\n"

floating :: Parser Double
floating =
  Unsafe.read <$> do
    lead <- integer
    char '.'
    decimals <- some digit
    pure (show lead ++ "." ++ decimals)

token :: Parser String
token = do
  firstChar <- alphabetic <|> char '_'
  rest <- many $ alphabetic <|> digit <|> char '_'
  return $ firstChar : rest

consumeRemaining :: Parser String
consumeRemaining = many (satisfy (const True))

bool :: Parser Bool
bool = (string "true" $> True) <|> (string "false" $> False)

bracketed :: Parser a -> Parser a
bracketed parser = do
  char '('
  many whitespace
  output <- parser
  many whitespace
  char ')'
  return output

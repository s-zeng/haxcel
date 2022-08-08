{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Data.Foldable
import Data.Functor

newtype Parser a = Parser {runParse :: String -> Maybe (a, String)} deriving (Functor)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $
  \case
    (c : cs) | pred c -> return (c, cs)
    _ -> empty

char :: Char -> Parser Char
char x = satisfy (== x)

eof :: Parser ()
eof = Parser $
  \case
    (c : cs) -> empty
    "" -> return ((), "")

instance Alternative Parser where
  empty = Parser $ pure empty
  f <|> g = Parser $ \s ->
    case runParse f s of
      Nothing -> runParse g s
      res -> res

instance Applicative Parser where
  pure x = Parser $ \s -> pure (x, s)
  p <*> q = Parser $ \s -> do
    (f, s1) <- runParse p s
    (a, s2) <- runParse q s1
    pure (f a, s2)

instance Monad Parser where
  return = pure
  p >>= q = Parser $ \s -> do
    (a, s') <- runParse p s
    runParse (q a) s'

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

isNot :: Parser a -> Parser ()
isNot f = Parser $ \s -> case runParse f s of
  Nothing -> pure ((), s)
  _ -> Nothing

parens :: Parser a -> Parser a
parens f = do
  char '('
  x <- f
  char ')'
  pure x

digit, leadingDigit :: Parser Char
digit = oneOfChar ['0' .. '9']
leadingDigit = oneOfChar ['1' .. '9']

natural :: Parser Integer
natural =
  read <$> do
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
    pure $ case minus of
      Nothing -> num
      Just _ -> negate num

whitespace :: Parser Char
whitespace = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

floating :: Parser Double
floating =
  read <$> do
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

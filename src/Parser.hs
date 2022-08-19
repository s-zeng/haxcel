-- | My implementation of Parser Combinators
module Parser where

import Relude
import qualified Relude.Unsafe as Unsafe

-- | A parser can be implemented as a monad transformer, giving us Applicative,
-- Monad and Alternative implementations for free!
-- The Applicative/Monad interface let's us write parsers in a very declarative
-- way with do-notation, and the Alternative interface lets us do disjunction on parsers
--
-- `StateT String Maybe a` is isomorphic to `String -> Maybe (a, String)`, so
-- essentially a parser takes a string, and if parse succeeds, returns a tuple
-- of the parsed object and the remaining string
newtype Parser a = Parser {getParser :: StateT String Maybe a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadFail
    )

runParse :: Parser a -> String -> Maybe (a, String)
runParse = runStateT . getParser

-- | The primitive for building a parser -- this matches a single character
-- that satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ do
  (c : cs) <- get
  guard $ pred c
  put cs
  return c

-- | Primitive parser that matches a single particular character
char :: Char -> Parser Char
char x = satisfy (== x)

-- | Succeeds iff the string to parse is now empty
eof :: Parser ()
eof = Parser $ do
  "" <- get
  return ()

-- | Parsing a string is parsing a sequence of characters
string :: String -> Parser String
string [] = pure []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

-- | Given a list of parsers, tries each parser in order and returns the first
-- successful parse if any
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

-- | Returns the entirety of the remaining unparsed String
consumeRemaining :: Parser String
consumeRemaining = many (satisfy (const True))

bool :: Parser Bool
bool = (string "true" $> True) <|> (string "false" $> False)

-- | Transforms a parser that parses string `k` into a parser that parses
-- string `(k)` (doesn't affect the returned object from the parser)
bracketed :: Parser a -> Parser a
bracketed parser = do
  char '('
  many whitespace
  output <- parser
  many whitespace
  char ')'
  return output

-- Transforms a Maybe into a pure (doesn't affect underlying string) parser
-- that either returns the given Just value, or fails
maybeToAlternative :: Alternative f => Maybe a -> f a
maybeToAlternative = \case
  Nothing -> empty
  Just x -> pure x

module Commands where

import Parser
import Relude

-- TODO: if we need row/column operations, turn cellName into a non-string type
data Command
  = Edit {cellName :: String, value :: String}
  | PrintCellRaw String
  | PrintValue String
  deriving (Show)

commandParser :: Parser Command
commandParser = editParser <|> printParser <|> valueParser
  where
    editParser = do
      char 'e'
      some whitespace
      cellName <- token
      some whitespace
      Edit cellName <$> consumeRemaining
    printParser = do
      char 'p'
      some whitespace
      PrintCellRaw <$> token
    valueParser = do
      char 'v'
      some whitespace
      PrintValue <$> token

{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Commands where

import Control.Applicative
import Parser

data Command = Edit {cellName :: String, value :: String} | PrintCellRaw String | PrintValue String deriving (Show)

commandParser :: Parser Command
commandParser = editParser <|> printParser <|> valueParser
  where
    editParser = do
      char 'e'
      many whitespace
      cellName <- token
      many whitespace
      value <- consumeRemaining
      return $ Edit cellName value
    printParser = do
      char 'p'
      many whitespace
      cellName <- token
      return $ PrintCellRaw cellName
    valueParser = do
      char 'v'
      many whitespace
      cellName <- token
      return $ PrintValue cellName

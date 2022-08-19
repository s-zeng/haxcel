module Commands where

import qualified Data.Map as Map
import ParseValue
import Parser
import Polysemy
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Relude hiding (State, get, modify)
import qualified Relude.Unsafe as Unsafe

-- | The AST for the CLI interface
data Command
  = Edit {cellName :: String, value :: String}
  | PrintCellRaw String
  | PrintValue String
  deriving (Show)

commandParser :: Parser Command
commandParser = oneOf [editParser, printParser, valueParser]
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

-- | The logic for the CLI prompt
-- We use polysemy to manage IO and state to make it easier to test --
-- capabilities like IO and starting state are abstracted
-- For instance, app/Main.hs runs this commandLine in the IO monad
-- whereas test/Spec.hs keeps this function pure with inputs/outputs from lists
commandLine :: Members '[State CellTable, Input (Maybe String), Output String, Fail] r => Sem r ()
commandLine = forever $ do
  output "> "
  command <- input
  case command of
    Nothing -> fail "end of input, goodbye!"
    Just c -> pure ()
  curTable <- get
  case runParse commandParser (Unsafe.fromJust command) of
    Just (Edit {cellName, value}, "") -> modify (Map.insert cellName value)
    Just (PrintCellRaw cellName, "") -> do
      case Map.lookup cellName curTable of
        Just value -> outputLn value
        Nothing -> outputLn "cell not found"
    Just (PrintValue cellName, "") -> do
      case Map.lookup cellName curTable of
        Just value -> case runParse (ParseValue.parser curTable) value of
          Just (result, "") -> outputLn (derivedValue result)
          Just _ -> outputLn "malformed or maltyped input..."
          Nothing -> outputLn "malformed or maltyped input"
        Nothing -> outputLn "cell not found"
    Just _ -> outputLn "error: bad command..."
    Nothing -> outputLn "error: bad command"
  where
    outputLn :: Member (Output String) r => String -> Sem r ()
    outputLn str = output str >> output "\n"

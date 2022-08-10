module Main where

import Commands
import qualified Data.Map as Map
import ParseValue
import Parser
import Relude

loop :: StateT CellTable IO ()
loop = forever $ do
  putStr "> "
  hFlush stdout
  command <- toString <$> getLine
  curTable <- get
  case runParse commandParser command of
    Just (Edit {cellName, value}, "") -> modify (Map.insert cellName value)
    Just (PrintCellRaw cellName, "") -> do
      case Map.lookup cellName curTable of
        Just value -> print value
        Nothing -> putStrLn "cell not found"
    Just (PrintValue cellName, "") -> do
      case Map.lookup cellName curTable of
        Just value -> case runParse (ParseValue.parser curTable) value of
          Just (result, "") -> putStrLn (derivedValue result)
          Just _ -> putStrLn "malformed or maltyped input..."
          Nothing -> putStrLn "malformed or maltyped input"
        Nothing -> putStrLn "cell not found"
    Just _ -> putStrLn "error: bad command..."
    Nothing -> putStrLn "error: bad command"

main :: IO ()
main = evalStateT loop Map.empty

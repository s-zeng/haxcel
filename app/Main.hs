{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Commands
import Data.Map (Map)
import qualified Data.Map as Map
import ParseValue
import Parser
import System.IO

type CellTable = Map String String

loop :: CellTable -> IO ()
loop table = do
  putStr "> "
  hFlush stdout
  command <- getLine
  newTable <- case runParse commandParser command of
    Just (Edit {cellName, value}, "") -> return $ Map.insert cellName value table
    Just (PrintCellRaw cellName, "") -> do
      case Map.lookup cellName table of
        Just value -> print value
        Nothing -> putStrLn "cell not found"
      return table
    Just (PrintValue cellName, "") -> do
      case Map.lookup cellName table of
        Just value -> case runParse (ParseValue.parser table) value of
          Just (result, "") -> putStrLn (derivedValue result)
          Just _ -> putStrLn "malformed or maltyped input..."
          Nothing -> putStrLn "malformed or maltyped input"
        Nothing -> putStrLn "cell not found"
      return table
    Just _ -> putStrLn "error: bad command..." >> return table
    Nothing -> putStrLn "error: bad command" >> return table
  loop newTable

main :: IO ()
main = loop Map.empty

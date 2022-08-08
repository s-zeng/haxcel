{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ParseValue where

import Control.Applicative
import Control.Monad
import Data.Coerce
import Data.Data
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Interp
import Parser

data ParsedValue = forall a. Show a => ParsedValue (Value a)

deriving instance Show ParsedValue

-- ugly :(
numericValue :: ParsedValue -> Maybe (Value Number)
numericValue (ParsedValue (BoolLiteral _)) = Nothing
numericValue (ParsedValue l@(IntLiteral _)) = Just l
numericValue (ParsedValue l@(FloatLiteral _)) = Just l
numericValue (ParsedValue (StringLiteral _)) = Nothing
numericValue (ParsedValue l@(InfixOp {})) = Just l
numericValue (ParsedValue l@(Sum _)) = Just l
numericValue (ParsedValue l@(If cond a b)) = do
  valA <- numericValue (ParsedValue a)
  valB <- numericValue (ParsedValue b)
  Just $ If cond valA valB

boolValue :: ParsedValue -> Maybe (Value Bool)
boolValue (ParsedValue l@(BoolLiteral _)) = Just l
boolValue (ParsedValue (IntLiteral _)) = Nothing
boolValue (ParsedValue (FloatLiteral _)) = Nothing
boolValue (ParsedValue (StringLiteral _)) = Nothing
boolValue (ParsedValue (InfixOp {})) = Nothing
boolValue (ParsedValue (Sum _)) = Nothing
boolValue (ParsedValue l@(If cond a b)) = do
  valA <- boolValue (ParsedValue a)
  valB <- boolValue (ParsedValue b)
  Just $ If cond valA valB

stringValue :: ParsedValue -> Maybe (Value String)
stringValue (ParsedValue (BoolLiteral _)) = Nothing
stringValue (ParsedValue (IntLiteral _)) = Nothing
stringValue (ParsedValue (FloatLiteral _)) = Nothing
stringValue (ParsedValue l@(StringLiteral _)) = Just l
stringValue (ParsedValue (InfixOp {})) = Nothing
stringValue (ParsedValue (Sum _)) = Nothing
stringValue (ParsedValue (If cond a b)) = do
  valA <- stringValue (ParsedValue a)
  valB <- stringValue (ParsedValue b)
  Just $ If cond valA valB

addOp, mulOp :: Parser Op
addOp = (char '+' $> Plus) <|> (char '-' $> Minus)
mulOp = char '*' $> Times

chainOp :: Parser (Value Number) -> Parser Op -> Parser (Value Number)
chainOp arg op = do
  first_arg <- arg
  rest <- many $ do
    o <- op
    a <- arg
    return $ \nextArg -> InfixOp o nextArg a
  return $ foldl' (&) first_arg rest

functionCall :: Parser (String, [ParsedValue])
functionCall = do
  functionName <- token
  char '('
  firstArg <- parser
  restOfArgs <- many $ do
    many whitespace
    char ','
    many whitespace
    nextArg <- parser
    return nextArg
  optional $ char ','
  char ')'
  return (functionName, firstArg : restOfArgs)

parser :: Parser ParsedValue
parser =
  oneOf
    [ ParsedValue <$> mathExpr,
      ParsedValue <$> sumFunction,
      ifExpr,
      ParsedValue <$> boolean,
      ParsedValue <$> int,
      ParsedValue <$> float,
      ParsedValue <$> str
    ]
  where
    boolean = BoolLiteral <$> bool
    int = IntLiteral <$> integer
    float = FloatLiteral <$> floating
    str = StringLiteral <$> consumeRemaining
    mathTerm =
      int
        <|> float
        <|> bracketed mathExpr
        <|> sumFunction
        <|> do
          ifVal <- ifExpr
          maybe empty return (numericValue ifVal)
    mathFactor = chainOp mathTerm mulOp
    mathExpr = chainOp mathFactor addOp
    sumFunction = do
      ~(name, args) <- functionCall
      let numericArgs = mapMaybe numericValue args
      guard $ length numericArgs == length args
      guard $ name == "sum"
      return $ Sum numericArgs
    ifExpr = do
      (name, args) <- functionCall
      guard $ name == "if"

      -- ugly :(
      case map boolValue args of
        [Just cond, Just a, Just b] -> return . ParsedValue $ If cond a b
        [Just cond, Nothing, Nothing] -> case mapMaybe numericValue args of
          [a, b] -> return . ParsedValue $ If cond a b
          _ -> case mapMaybe stringValue args of
            [a, b] -> return . ParsedValue $ If cond a b
            _ -> empty
        _ -> empty

derivedValue :: ParsedValue -> String
derivedValue (ParsedValue p) = show $ interp p

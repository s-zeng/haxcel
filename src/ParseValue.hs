module ParseValue where

import qualified Data.Map as Map
import Interp
import Parser
import Relude hiding (Op, Sum, bool)

data ParsedValue = forall a. Show a => ParsedValue (Value a)

deriving instance Show ParsedValue

extract :: (forall a. Value a -> Maybe (Value b)) -> ParsedValue -> Maybe (Value b)
extract extractor (ParsedValue p) = extractor p

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

type CellTable = Map String String

functionCall :: CellTable -> Parser (String, [ParsedValue])
functionCall cellTable = do
  functionName <- token
  bracketed $ do
    firstArg <- parser cellTable
    restOfArgs <- many $ do
      many whitespace
      char ','
      many whitespace
      parser cellTable
    optional $ char ','
    return (functionName, firstArg : restOfArgs)

parser :: CellTable -> Parser ParsedValue
parser cellTable =
  oneOf
    [ ParsedValue <$> mathExpr,
      ParsedValue <$> sumFunction,
      ifExpr,
      ParsedValue <$> boolean,
      cellRef,
      ParsedValue <$> str
    ]
  where
    float = FloatLiteral <$> floating
    boolean = BoolLiteral <$> bool
    int = IntLiteral <$> integer
    str = StringLiteral <$> consumeRemaining
    cellRef = do
      cellName <- token
      fromMaybe empty $ do
        val <- Map.lookup cellName cellTable
        (res, _) <- runParse (parser cellTable) val
        Just $ return res
    mathTerm =
      oneOf
        [ float,
          int,
          bracketed mathExpr,
          sumFunction,
          do
            val <- ifExpr <|> cellRef
            maybe empty return (extract numericValue val)
        ]
    mathFactor = chainOp mathTerm mulOp
    mathExpr = chainOp mathFactor addOp
    sumFunction = do
      (name, args) <- functionCall cellTable
      let numericArgs = mapMaybe (extract numericValue) args
      guard $ length numericArgs == length args
      guard $ name == "sum"
      return $ Sum numericArgs
    ifExpr = do
      (name, args) <- functionCall cellTable
      guard $ name == "if"

      -- ugly :(
      case map (extract boolValue) args of
        [Just cond, Just a, Just b] -> return . ParsedValue $ If cond a b
        [Just cond, Nothing, Nothing] -> case mapMaybe (extract numericValue) args of
          [a, b] -> return . ParsedValue $ If cond a b
          _ -> case mapMaybe (extract stringValue) args of
            [a, b] -> return . ParsedValue $ If cond a b
            _ -> empty
        _ -> empty

derivedValue :: ParsedValue -> String
derivedValue (ParsedValue p) = show $ interp p

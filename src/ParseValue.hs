-- | This is the meat of the spreadsheet implementation -- parsing the contents of the cells!
module ParseValue where

import qualified Data.Map as Map
import Interp
import Parser
import Relude hiding (Op, Sum, bool)

-- | We wrap a Value into an existential type so that we can write one parser for all the possible Value types
data ParsedValue = forall a. (Show a, Eq a) => ParsedValue (Value a)

deriving instance Show ParsedValue

-- | To get a Value out of a ParsedValue, we need to pass it a function that
-- could extract a specific Value type out of any Value. Hence, this needs Rank 2 types
extract :: forall b. (forall a. Value a -> Maybe (Value b)) -> ParsedValue -> Maybe (Value b)
extract extractor (ParsedValue p) = extractor p

addOp, mulOp :: Parser Op
addOp = (char '+' $> Plus) <|> (char '-' $> Minus)
mulOp = char '*' $> Times

-- | Lets us chain together arithmetic parsers
chainOp :: Parser (Value Number) -> Parser Op -> Parser (Value Number)
chainOp arg op = do
  first_arg <- arg
  rest <- many $ do
    o <- op
    a <- arg
    return $ \nextArg -> InfixOp o nextArg a
  return $ foldl' (&) first_arg rest

type CellTable = Map String String

-- Parses a function name as well as the list of arguments passed to the function
-- requires a celltable since the main parser requires a celltable, and this calls the main parser
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


ifExpr' cellTable = do
  (name, [condExpr, arg1, arg2]) <- functionCall cellTable
  guard $ name == "if"
  cond <- maybeToAlternative $ extract boolValue condExpr
  -- returns a valid If expr iff the two args have the same type
  maybeToAlternative $
    asum
      [ do
          a <- extract boolValue arg1
          b <- extract boolValue arg2
          return . ParsedValue $ If cond a b,
        do
          a <- extract numericValue arg1
          b <- extract numericValue arg2
          return . ParsedValue $ If cond a b,
        do
          a <- extract stringValue arg1
          b <- extract stringValue arg2
          return . ParsedValue $ If cond a b
      ]

parser :: CellTable -> Parser ParsedValue
parser cellTable =
  oneOf
    [ ParsedValue <$> mathExpr,
      ParsedValue <$> sumFunction,
      equalFunction,
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
      maybeToAlternative $ do
        val <- Map.lookup cellName cellTable
        (res, _) <- runParse (parser cellTable) val
        return res
    mathTerm =
      oneOf
        [ float,
          int,
          bracketed mathExpr,
          sumFunction,
          do
            -- val <- (ifExpr :: Parser ParsedValue) <|> cellRef
            val <- cellRef
            maybeToAlternative (extract numericValue val)
        ]
    mathFactor = chainOp mathTerm mulOp
    mathExpr = chainOp mathFactor addOp
    -- TODO: write a destructuring free monad to simplify parsing of functions?
    -- there's a good amount of stuff we can reuse
    sumFunction = do
      (name, args) <- functionCall cellTable
      numericArgs <- maybeToAlternative $ traverse (extract numericValue) args
      guard $ name == "sum"
      return $ Sum numericArgs

    ifExpr = do
      (name, [condExpr, arg1, arg2]) <- functionCall cellTable
      guard $ name == "if"
      cond <- maybeToAlternative $ extract boolValue condExpr
      -- returns a valid If expr iff the two args have the same type
      maybeToAlternative $
        asum
          [ do
              a <- extract boolValue arg1
              b <- extract boolValue arg2
              return . ParsedValue $ If cond a b,
            do
              a <- extract numericValue arg1
              b <- extract numericValue arg2
              return . ParsedValue $ If cond a b,
            do
              a <- extract stringValue arg1
              b <- extract stringValue arg2
              return . ParsedValue $ If cond a b
          ]

    equalFunction = do
      (name, [arg1, arg2]) <- functionCall cellTable
      guard $ name == "equal"
      -- TODO: figure out the type magic to deduplicate this block
      maybeToAlternative $
        asum
          [ do
              a <- extract boolValue arg1
              b <- extract boolValue arg2
              return . ParsedValue $ Equal a b,
            do
              a <- extract numericValue arg1
              b <- extract numericValue arg2
              return . ParsedValue $ Equal a b,
            do
              a <- extract stringValue arg1
              b <- extract stringValue arg2
              return . ParsedValue $ Equal a b
          ]

-- | Interprets the parsed AST
-- We can't write a function `ParsedValue -> Value a`, because of the `forall`
-- in the type deifnition of ParsedValue. But the type also guarantees we only
-- parse types that have Show instances, so we can extract a value from a
-- ParsedValue if we interpret it and turn it into a string first
derivedValue :: ParsedValue -> String
derivedValue (ParsedValue p) = show $ interp p

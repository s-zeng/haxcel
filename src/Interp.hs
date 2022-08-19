module Interp where

import Relude hiding (Op, Sum)

data Op = Plus | Minus | Times deriving (Show)

data Number = Int Integer | Floating Double deriving (Show, Eq)

-- | This is the core AST of the spreadsheet formulas, implemented as a GADT
-- A spreadsheet value can either be a primitive (bool, int, float, string),
-- or a math formula, or one of the built in functions
-- There's no implementation of lambdas, and typechecking essentially happens
-- at the parse level currently, so in order to define new functions you must
-- add them here with the appropriate signature
data Value a where
  BoolLiteral :: Bool -> Value Bool
  IntLiteral :: Integer -> Value Number
  FloatLiteral :: Double -> Value Number
  StringLiteral :: String -> Value String
  InfixOp :: Op -> Value Number -> Value Number -> Value Number
  Sum :: [Value Number] -> Value Number
  If :: Value Bool -> Value a -> Value a -> Value a
  Equal :: (Show a, Eq a) => Value a -> Value a -> Value Bool

deriving instance Show a => Show (Value a)

-- | ugly :(
-- We define functions to check the type of a Value by pattern matching on every case
boolValue :: Value a -> Maybe (Value Bool)
stringValue :: Value a -> Maybe (Value String)
numericValue :: Value a -> Maybe (Value Number)
boolValue l@(BoolLiteral _) = Just l
boolValue l@(Equal _ _) = Just l
boolValue l@(If cond a b) = If cond <$> boolValue a <*> boolValue b
boolValue _ = Nothing

stringValue l@(StringLiteral _) = Just l
stringValue l@(If cond a b) = If cond <$> stringValue a <*> stringValue b
stringValue _ = Nothing

numericValue l@(InfixOp {}) = Just l
numericValue l@(Sum _) = Just l
numericValue l@(IntLiteral _) = Just l
numericValue l@(FloatLiteral _) = Just l
numericValue l@(If cond a b) = If cond <$> numericValue a <*> numericValue b
numericValue _ = Nothing

-- sameTyped ::
-- sameTyped arg1 arg2 =
--   asum
--     [ do
--         a <- boolValue arg1
--         b <- boolValue arg2
--         return (a, b),
--       do
--         a <- numericValue arg1
--         b <- numericValue arg2
--         return (a, b),
--       do
--         a <- stringValue arg1
--         b <- stringValue arg2
--         return (a, b)
--     ]

translateOp :: Num a => Op -> a -> a -> a
translateOp Plus = (+)
translateOp Minus = (-)
translateOp Times = (*)

-- | Logic to handle mixed floating/integer arithmetic
runOp :: Op -> Number -> Number -> Number
runOp op (Int a) (Int b) = Int $ translateOp op a b
runOp op (Floating a) (Floating b) = Floating $ translateOp op a b
runOp op (Int a) (Floating b) = Floating $ translateOp op (fromInteger a) b
runOp op (Floating a) (Int b) = Floating $ translateOp op a (fromInteger b)

-- | The reason we use a GADT for this: a very elegant interpreter implementation!
interp :: Eq a => Value a -> a
interp (BoolLiteral b) = b
interp (IntLiteral i) = Int i
interp (FloatLiteral f) = Floating f
interp (StringLiteral s) = s
interp (InfixOp op numA numB) = runOp op (interp numA) (interp numB)
interp (Sum vals) = foldl' (runOp Plus) (Int 0) $ map interp vals
interp (If cond a b) = if interp cond then interp a else interp b
interp (Equal a b) = interp a == interp b

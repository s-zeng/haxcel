module Interp where

import Relude hiding (Op, Sum)

data Op = Plus | Minus | Times deriving (Show)

data Number = Int Integer | Floating Double deriving (Show)

data Value a where
  BoolLiteral :: Bool -> Value Bool
  IntLiteral :: Integer -> Value Number
  FloatLiteral :: Double -> Value Number
  StringLiteral :: String -> Value String
  InfixOp :: Op -> Value Number -> Value Number -> Value Number
  -- write a destructuring free monad?
  Sum :: [Value Number] -> Value Number
  If :: Value Bool -> Value a -> Value a -> Value a

deriving instance Show a => Show (Value a)

boolValue :: Value a -> Maybe (Value Bool)
boolValue l@(BoolLiteral _) = Just l
boolValue l@(If cond a b) = If cond <$> boolValue a <*> boolValue b
boolValue _ = Nothing

stringValue :: Value a -> Maybe (Value String)
stringValue l@(StringLiteral _) = Just l
stringValue l@(If cond a b) = If cond <$> stringValue a <*> stringValue b
stringValue _ = Nothing

-- ugly :(
numericValue :: Value a -> Maybe (Value Number)
numericValue l@(InfixOp {}) = Just l
numericValue l@(Sum _) = Just l
numericValue l@(IntLiteral _) = Just l
numericValue l@(FloatLiteral _) = Just l
numericValue l@(If cond a b) = If cond <$> numericValue a <*> numericValue b
numericValue _ = Nothing

translateOp :: Num a => Op -> a -> a -> a
translateOp Plus = (+)
translateOp Minus = (-)
translateOp Times = (*)

runOp :: Op -> Number -> Number -> Number
runOp op (Int a) (Int b) = Int $ translateOp op a b
runOp op (Floating a) (Floating b) = Floating $ translateOp op a b
runOp op (Int a) (Floating b) = Floating $ translateOp op (fromInteger a) b
runOp op (Floating a) (Int b) = Floating $ translateOp op a (fromInteger b)

interp :: Value a -> a
interp (BoolLiteral b) = b
interp (IntLiteral i) = Int i
interp (FloatLiteral f) = Floating f
interp (StringLiteral s) = s
interp (InfixOp op numA numB) = runOp op (interp numA) (interp numB)
interp (Sum vals) = foldl' (runOp Plus) (Int 0) $ map interp vals
interp (If cond a b) = if interp cond then interp a else interp b

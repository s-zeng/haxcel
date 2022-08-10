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

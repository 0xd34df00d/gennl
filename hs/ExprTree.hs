module ExprTree
    where

type Const = Double

data Var = Var String
    deriving (Show)

data UnaryFunc = Sin | Cos | Log
    deriving (Show)

data BinaryFunc = Plus | Minus | Mul | Div | Pow
    deriving (Show)

data ExprTree = NodeUnary UnaryFunc ExprTree
                | NodeBinary BinaryFunc ExprTree ExprTree
                | LeafVar Var
                | LeafConst Const
    deriving (Show)

module ExprTree
    (
        ExprTree,
        intLeaf,
        realLeaf,
        varLeaf,
        unaryNode,
        binaryNode
    )
    where

import Control.Monad (liftM)

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

intLeaf = LeafConst . fromInteger
realLeaf = LeafConst

varLeaf = LeafVar . Var

unaryNode s = NodeUnary `liftM` lookup s al 
    where al = [ ("sin", Sin), ("cos", Cos), ("log", Log) ]

binaryNode s = NodeBinary `liftM` lookup s al
    where al = [ ("+", Plus), ("-", Minus), ("*", Mul), ("/", Div)]

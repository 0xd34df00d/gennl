module ExprTree
    (
        ExprTree(..),
        UnaryFunc(..),
        BinaryFunc(..),
        Var(..),
        Const,
        intLeaf,
        realLeaf,
        varLeaf,
        unaryNode,
        binaryNode,
        simplifyTree,
        simplifyStab
    )
    where

import Control.Monad (liftM)
import SupportUtils

type Const = Double

data Var = Var String
    deriving (Show, Eq)

unaryOps = [ (Sin, sin), (Cos, cos), (Log, log) ]

data UnaryFunc = Sin | Cos | Log
    deriving (Show, Eq)

binaryOps = [ (Plus, (+)), (Minus, (-)), (Mul, (*)), (Div, (/)), (Pow, (**)) ]

data BinaryFunc = Plus | Minus | Mul | Div | Pow
    deriving (Show, Eq)

data ExprTree = NodeUnary UnaryFunc ExprTree
                | NodeBinary BinaryFunc ExprTree ExprTree
                | LeafVar Var
                | LeafConst Const
    deriving (Show, Eq)

intLeaf = LeafConst . fromInteger
realLeaf = LeafConst

varLeaf = LeafVar . Var

unaryNode s = NodeUnary `liftM` lookup s al 
    where al = [ ("sin", Sin), ("cos", Cos), ("log", Log) ]

binaryNode s = NodeBinary `liftM` lookup s al
    where al = [ ("+", Plus), ("-", Minus), ("*", Mul), ("/", Div), ("^", Pow)]

-- Better to place terminating optimizations at the top, obviously
simplifyTree :: ExprTree -> ExprTree
simplifyTree (NodeBinary Pow _ (LeafConst 0.0)) = LeafConst 1.0
simplifyTree (NodeBinary Pow l@(LeafConst 1.0) _) = l
simplifyTree (NodeBinary Mul l@(LeafConst 0.0) _) = l
simplifyTree (NodeBinary Mul _ l@(LeafConst 0.0)) = l
simplifyTree (NodeUnary a (LeafConst x)) = LeafConst $ maybe fail ($ x) (lookup a unaryOps)
    where fail = failStr $ "Unable to find unary function " ++ show a
simplifyTree (NodeBinary a (LeafConst x) (LeafConst y)) = LeafConst $ maybe fail (\op -> x `op` y) (lookup a binaryOps)
    where fail = failStr $ "Unable to find binary function " ++ show a
simplifyTree (NodeBinary Pow a (LeafConst 1.0)) = simplifyTree a
simplifyTree (NodeBinary Mul (LeafConst 1.0) a) = simplifyTree a
simplifyTree (NodeBinary Mul a (LeafConst 1.0)) = simplifyTree a
simplifyTree (NodeBinary f a b) = NodeBinary f (simplifyTree a) (simplifyTree b)
simplifyTree (NodeUnary f a) = NodeUnary f (simplifyTree a)
simplifyTree t = t

simplifyStab t = if t == t' then t else simplifyStab t'
    where t' = simplifyTree t

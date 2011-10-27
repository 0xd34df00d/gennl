module Funcs
    where

import Formattable

data BinFuncInfo = BinFuncInfo {
        comm :: Bool
    }
    deriving (Show, Eq)

data UnaryFunc = Sin | Cos | Log
    deriving (Show, Eq)

instance Formattable UnaryFunc where
    pretty Sin = "sin"
    pretty Cos = "cos"
    pretty Log = "log"

unaryOps :: Floating a => [(UnaryFunc, a -> a)]
unaryOps = [ (Sin, sin), (Cos, cos), (Log, log) ]

unaryOpsOnly = map fst unaryOps

data BinaryFunc = Plus | Minus | Mul | Div | Pow
    deriving (Show, Eq)

instance Formattable BinaryFunc where
    pretty Plus = "+"
    pretty Minus = "-"
    pretty Mul = "*"
    pretty Div = "/"
    pretty Pow = "**"

binaryOps :: Floating a => [(BinaryFunc, a -> a -> a)]
binaryOps = [ (Plus, (+)), (Minus, (-)), (Mul, (*)), (Div, (/)), (Pow, (**)) ]

binaryOpsOnly = map fst binaryOps

binFuncInfos :: [(BinaryFunc, BinFuncInfo)]
binFuncInfos = [
                    (Plus, BinFuncInfo True),
                    (Minus, BinFuncInfo False),
                    (Mul, BinFuncInfo True),
                    (Div, BinFuncInfo False),
                    (Pow, BinFuncInfo False)
               ]

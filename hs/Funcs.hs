module Funcs
    where

import Data.List

import Formattable

data BinFuncInfo = BinFuncInfo {
        comm :: Bool,
        assoc :: Bool
    }
    deriving (Show, Eq)

data UnaryFunc = Sin | Cos | Log
    deriving (Show, Eq, Ord)

instance Formattable UnaryFunc where
    pretty Sin = "sin"
    pretty Cos = "cos"
    pretty Log = "log"

unaryOps :: Floating a => [(UnaryFunc, a -> a)]
unaryOps = [ (Sin, sin), (Cos, cos), (Log, log) ]

unaryOpsOnly = map fst unaryOps

data BinaryFunc = Plus | Minus | Mul | Div | Pow
    deriving (Show, Eq, Ord)

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
                    (Plus, BinFuncInfo True True),
                    (Minus, BinFuncInfo False False),
                    (Mul, BinFuncInfo True True),
                    (Div, BinFuncInfo False False),
                    (Pow, BinFuncInfo False False)
               ]
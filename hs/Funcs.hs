module Funcs
    where

import Data.List

import Formattable

data UnaryFunc = Sin | Cos | Log | Tan | Asin | Acos | Atan
    deriving (Show, Eq, Ord)

instance Formattable UnaryFunc where
    pretty Sin = "sin"
    pretty Cos = "cos"
    pretty Log = "log"
    pretty Tan = "tan"
    pretty Asin = "asin"
    pretty Acos = "acos"
    pretty Atan = "atan"

unaryOps :: Floating a => UnaryFunc -> a -> a
unaryOps Sin = sin
unaryOps Cos = cos
unaryOps Log = log
unaryOps Tan = tan
unaryOps Asin = asin
unaryOps Acos = acos
unaryOps Atan = atan
{-# SPECIALIZE unaryOps :: UnaryFunc -> Double -> Double #-}

unaryOpsOnly = [ Sin, Cos, Log, Tan, Asin, Acos, Atan ]

data BinaryFunc = Plus | Minus | Mul | Div | Pow
    deriving (Show, Eq, Ord)

instance Formattable BinaryFunc where
    pretty Plus = "+"
    pretty Minus = "-"
    pretty Mul = "*"
    pretty Div = "/"
    pretty Pow = "**"

binaryOps :: Floating a => BinaryFunc -> a -> a -> a
binaryOps Plus = (+)
binaryOps Minus = (-)
binaryOps Mul = (*)
binaryOps Div = (/)
binaryOps Pow = (**)
{-# SPECIALIZE binaryOps :: BinaryFunc -> Double -> Double -> Double #-}

binaryOpsOnly = [ Plus, Minus, Mul, Div, Pow ]

data BinFuncInfo = BinFuncInfo {
        comm :: Bool,
        assoc :: Bool
    }
    deriving (Show, Eq)

binFInf :: BinaryFunc -> BinFuncInfo
binFInf Plus = BinFuncInfo True True
binFInf Minus = BinFuncInfo False False
binFInf Mul = BinFuncInfo True True
binFInf Div = BinFuncInfo False False
binFInf Pow = BinFuncInfo False False

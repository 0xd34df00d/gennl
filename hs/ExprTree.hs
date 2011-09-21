{-# LANGUAGE ScopedTypeVariables #-}

module ExprTree
    where

import Data.Functor ((<$>))
import SupportUtils
import Random
import Control.Arrow
import Data.List

type Const = Double

data Var = Var String
    deriving (Show, Eq)

data UnaryFunc = Sin | Cos | Log
    deriving (Show, Eq)

unaryOps = [ (Sin, sin), (Cos, cos), (Log, log) ]

unaryOpsOnly = map fst unaryOps

data BinaryFunc = Plus | Minus | Mul | Div | Pow
    deriving (Show, Eq)

binaryOps = [ (Plus, (+)), (Minus, (-)), (Mul, (*)), (Div, (/)), (Pow, (**)) ]

binaryOpsOnly = map fst binaryOps

data ExprTree = NodeUnary UnaryFunc ExprTree
                | NodeBinary BinaryFunc ExprTree ExprTree
                | LeafVar Var
                | LeafConst Const
    deriving (Show, Eq)

intLeaf = LeafConst . fromInteger
realLeaf = LeafConst

varLeaf = LeafVar . Var

unaryNode s = NodeUnary <$> lookup s al 
    where al = [ ("sin", Sin), ("cos", Cos), ("log", Log) ]

binaryNode s = NodeBinary <$> lookup s al
    where al = [ ("+", Plus), ("-", Minus), ("*", Mul), ("/", Div), ("^", Pow)]

randExprTree :: (RandomGen g) => [String] -> Int -> g -> (ExprTree, g)
randExprTree vars cpx g = randExprTree' vars g (0, cpx)

randExprTree' :: (RandomGen g) => [String] -> g -> (Int, Int) -> (ExprTree, g)
randExprTree' vars g (dh, cpx) | dh /= 0 && thr dice 0.12 0.30 = (LeafConst (dice * 50), g5)
                               | dh /= 0 && thr dice 0.30 0.30 = (LeafVar $ Var $ randElem vars g2, g5)
                               | dice <= 0.60 = (NodeBinary
                                                    (randElem binaryOpsOnly g2)
                                                    (fst $ randExprTree' vars g3 (dh + 1, cpx))
                                                    (fst $ randExprTree' vars g4 (dh + 1, cpx)),
                                                 g5)
                               | otherwise = (NodeUnary
                                                    (randElem unaryOpsOnly g2)
                                                    (fst $ randExprTree' vars g3 (dh + 1, cpx)),
                                                 g5)
    where (dice :: Double, _) = random g1
          (g0:g1:g2:g3:g4:g5:_) = take 6 (rndGens g)
          randElem xs g = xs !! fst (randomR (0, length xs - 1) g)
          (?) a b c | a = b
                    | otherwise = c
          thr dice t m = dice <= ((dh < cpx) ? t $ t / m)

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

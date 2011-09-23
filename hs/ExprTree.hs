{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module ExprTree
    where

import Control.Parallel
import Control.Parallel.Strategies
import Data.Functor ((<$>))
import Random
import Control.Arrow
import Data.List

import SupportUtils
import FormatClass

type Const = Double

data Var = Var String
    deriving (Show, Eq)

data UnaryFunc = Sin | Cos | Log
    deriving (Show, Eq)

instance Formattable UnaryFunc where
    pretty Sin = "sin"
    pretty Cos = "cos"
    pretty Log = "log"

unaryOps = [ (Sin, sin), (Cos, cos), (Log, log) ]

unaryOpsOnly = map fst unaryOps

data BinaryFunc = Plus | Minus | Mul | Div | Pow
    deriving (Show, Eq)

instance Formattable BinaryFunc where
    pretty Plus = "+"
    pretty Minus = "-"
    pretty Mul = "*"
    pretty Div = "/"
    pretty Pow = "^"

binaryOps = [ (Plus, (+)), (Minus, (-)), (Mul, (*)), (Div, (/)), (Pow, (**)) ]

binaryOpsOnly = map fst binaryOps

data ExprTree = NodeUnary !UnaryFunc !ExprTree
                | NodeBinary !BinaryFunc !ExprTree !ExprTree
                | LeafVar !Var
                | LeafConst !Const
    deriving (Show, Eq)

instance Formattable ExprTree where
    pretty (LeafVar (Var x)) = x
    pretty (LeafConst c) = show c
    pretty (NodeUnary f t) = pretty f ++ " (" ++ pretty t ++ ")"
    pretty (NodeBinary f l r) = "(" ++ pretty l ++ pretty f ++ pretty r ++ ")"

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

numNodes :: ExprTree -> Int
numNodes (LeafVar _) = 1
numNodes (LeafConst _) = 1
numNodes (NodeUnary _ t) = 1 + numNodes t
numNodes (NodeBinary _ l r) = 1 + numNodes l + numNodes r

evalTree :: [(String, Double)] -> ExprTree -> Double
evalTree _ !(LeafConst !c) = c
evalTree !vars !(LeafVar !(Var !v)) | Just !c <- lookup v vars = c
                                   | otherwise = error $ "Unknown var " ++ v
evalTree !vars !(NodeUnary !f !t) | Just !f' <- lookup f unaryOps = f' $ evalTree vars t
                                  | otherwise = error $ "Unknown uf " ++ show f
evalTree !vars !(NodeBinary !f !l !r) | Just !f' <- lookup f binaryOps = f' (evalTree vars l) (evalTree vars r)
                                      | otherwise = error $ "Unknown bf " ++ show f

atNodeBin :: (Int -> Int -> ExprTree -> ExprTree) -> Int -> Int -> (ExprTree, ExprTree) -> ExprTree
atNodeBin f i n (l, r) | nl +i >= n = f (i + 1) n l
                       | otherwise = f (i + nl + 1) n r
                          where nl = numNodes l

atNodeBin2 :: (Int -> Int -> ExprTree -> ExprTree) -> Int -> Int -> (ExprTree, ExprTree) -> (ExprTree, ExprTree)
atNodeBin2 f i n (l, r) | nl +i >= n = (f (i + 1) n l, r)
                        | otherwise = (l, f (i + nl + 1) n r)
                          where nl = numNodes l

subTree :: Int -> ExprTree -> ExprTree
subTree = subTree' 0

subTree' :: Int -> Int -> ExprTree -> ExprTree
subTree' i n t | i == n = t
subTree' i n (LeafVar _) = walkFail "subTree: var node" i n
subTree' i n (LeafConst _) = walkFail "subTree: const node" i n
subTree' i n (NodeBinary _ l r) = atNodeBin subTree' i n (l, r)
subTree' i n (NodeUnary _ t) = subTree' (i + 1) n t

repSubTree :: Int -> ExprTree -> ExprTree -> ExprTree
repSubTree = repSubTree' 0

repSubTree' :: Int -> Int -> ExprTree -> ExprTree -> ExprTree
repSubTree' i n r _ | i == n = r
repSubTree' i n r (LeafVar _) = walkFail "repSubTree: var node" i n
repSubTree' i n r (LeafConst _) = walkFail "repSubTree: const node" i n
repSubTree' i n r (NodeBinary f t1 t2) = NodeBinary f t1' t2'
    where (t1', t2') = atNodeBin2 (\i n t -> repSubTree' i n r t) i n (t1, t2)
repSubTree' i n r (NodeUnary f t) = NodeUnary f (repSubTree' (i + 1) n r t)

walkFail :: String -> Int -> Int -> a
walkFail s i n = error $ s ++ "; i = " ++ show i ++ "; n = " ++ show n

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

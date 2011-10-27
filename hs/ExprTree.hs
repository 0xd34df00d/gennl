{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExprTree
    where

import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Monad
import Control.Arrow
import Data.Functor ((<$>))
import Random
import Control.Arrow
import Data.List
import Numeric.FAD
import GHC.Float
import Debug.Trace

import SupportUtils
import Funcs
import Formattable

data Var = Var String
    deriving (Show, Eq)

data ExprTree a = NodeUnary !UnaryFunc !(ExprTree a)
                | NodeBinary !BinaryFunc !(ExprTree a) !(ExprTree a)
                | LeafVar !Var
                | LeafConst !a
    deriving (Show, Eq)

instance NFData a => NFData (ExprTree a)

class (Fractional a, Random a, Ord a, Eq a) => SuitableConst a
instance (Fractional a, Random a, Ord a, Eq a) => SuitableConst a

instance (Show a) => Formattable (ExprTree a) where
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

randExprTree :: (RandomGen g, SuitableConst a) => [String] -> Int -> g -> (ExprTree a, g)
randExprTree vars cpx g = randExprTree' vars g (0, cpx)

randExprTree' :: (RandomGen g, SuitableConst a) => [String] -> g -> (Int, Int) -> (ExprTree a, g)
randExprTree' vars g (dh, cpx) | dh /= 0 && thr dice 0.15 0.30 = (LeafConst (dice * 50), g5)
                               | dh /= 0 && thr dice 0.30 0.30 = (LeafVar $ Var $ randElem vars g2, g5)
                               | dice <= 0.98 = (NodeBinary
                                                    (randElem binaryOpsOnly g2)
                                                    (fst $ randExprTree' vars g3 (dh + 1, cpx))
                                                    (fst $ randExprTree' vars g4 (dh + 1, cpx)),
                                                 g5)
                               | otherwise = (NodeUnary
                                                    (randElem unaryOpsOnly g2)
                                                    (fst $ randExprTree' vars g3 (dh + 1, cpx)),
                                                 g5)
    where (dice, _) = random g1
          (g0:g1:g2:g3:g4:g5:_) = take 6 (rndGens g)
          randElem xs g = xs !! fst (randomR (0, length xs - 1) g)
          (?) a b c | a = b
                    | otherwise = c
          thr dice t m = dice <= ((dh < cpx) ? t $ t / m)

numNodes :: ExprTree a -> Int
numNodes (LeafVar _) = 1
numNodes (LeafConst _) = 1
numNodes (NodeUnary _ t) = 1 + numNodes t
numNodes (NodeBinary _ l r) = 1 + numNodes l + numNodes r

evalTree :: Floating a => [(String, a)] -> ExprTree a -> a
evalTree _ !(LeafConst !c) = c
evalTree !vars !(LeafVar !(Var !v)) | Just !c <- lookup v vars = c
                                    | otherwise = error $ "Unknown var " ++ v
evalTree !vars !(NodeUnary !f !t) | Just !f' <- lookup f unaryOps = f' $ evalTree vars t
                                  | otherwise = error $ "Unknown uf " ++ show f
evalTree !vars !(NodeBinary !f !l !r) | Just !f' <- lookup f binaryOps = f' (evalTree vars l) (evalTree vars r)
                                      | otherwise = error $ "Unknown bf " ++ show f

atNodeBin :: (Int -> Int -> ExprTree a -> ExprTree a) -> Int -> Int -> (ExprTree a, ExprTree a) -> ExprTree a
atNodeBin f i n (l, r) | nl +i >= n = f (i + 1) n l
                       | otherwise = f (i + nl + 1) n r
                          where nl = numNodes l

atNodeBin2 :: (Int -> Int -> ExprTree a -> ExprTree a) -> Int -> Int -> (ExprTree a, ExprTree a) -> (ExprTree a, ExprTree a)
atNodeBin2 f i n (l, r) | nl +i >= n = (f (i + 1) n l, r)
                        | otherwise = (l, f (i + nl + 1) n r)
                          where nl = numNodes l

subTree :: Int -> ExprTree a -> ExprTree a
subTree = subTree' 0

subTree' :: Int -> Int -> ExprTree a -> ExprTree a
subTree' i n t | i == n = t
subTree' i n (LeafVar _) = walkFail "subTree: var node" i n
subTree' i n (LeafConst _) = walkFail "subTree: const node" i n
subTree' i n (NodeBinary _ l r) = atNodeBin subTree' i n (l, r)
subTree' i n (NodeUnary _ t) = subTree' (i + 1) n t

repSubTree :: Int -> ExprTree a -> ExprTree a -> ExprTree a
repSubTree = repSubTree' 0

repSubTree' :: Int -> Int -> ExprTree a -> ExprTree a -> ExprTree a
repSubTree' i n r _ | i == n = r
repSubTree' i n r (LeafVar _) = walkFail "repSubTree: var node" i n
repSubTree' i n r (LeafConst _) = walkFail "repSubTree: const node" i n
repSubTree' i n r (NodeBinary f t1 t2) = NodeBinary f t1' t2'
    where (t1', t2') = atNodeBin2 (\i n t -> repSubTree' i n r t) i n (t1, t2)
repSubTree' i n r (NodeUnary f t) = NodeUnary f (repSubTree' (i + 1) n r t)

varTreeConsts :: ExprTree a -> (ExprTree a, [(String, a)])
varTreeConsts = (\(x, y, z) -> (x, y)) . varTreeConsts' 0

varTreeConsts' :: Int -> ExprTree a -> (ExprTree a, [(String, a)], Int)
varTreeConsts' n t@(LeafVar _) = (t, [], n)
varTreeConsts' n t@(LeafConst c) = (LeafVar (Var name), [(name, c)], n + 1)
    where name = '_' : show n
varTreeConsts' n (NodeUnary f t) = (\(x, y, z) -> (NodeUnary f x, y, z)) (varTreeConsts' n t)
varTreeConsts' n (NodeBinary f l r) = (NodeBinary f l' r', lrep ++ rrep, rpos)
    where (l', lrep, lpos) = varTreeConsts' n l
          (r', rrep, rpos) = varTreeConsts' lpos r

fixTreeVars :: [(String, a)] -> ExprTree a -> ExprTree a
fixTreeVars vals t@(LeafConst _) = t
fixTreeVars vals t@(LeafVar (Var v)) | Just v' <- lookup v vals = LeafConst v'
                                     | otherwise = t
fixTreeVars vals (NodeUnary f t) = NodeUnary f (fixTreeVars vals t)
fixTreeVars vals (NodeBinary f l r) = NodeBinary f (fixTreeVars vals l) (fixTreeVars vals r)

morphTreeConsts :: (a -> b) -> ExprTree a -> ExprTree b
morphTreeConsts c (LeafConst ct) = LeafConst $ c ct
morphTreeConsts _ (LeafVar v) = LeafVar v
morphTreeConsts c (NodeUnary f t) = NodeUnary f (morphTreeConsts c t)
morphTreeConsts c (NodeBinary f l r) = NodeBinary f (morphTreeConsts c l) (morphTreeConsts c r)

varredTreeJac :: (Real a, Floating a, Fractional a) => ExprTree a -> ([String], [String]) -> [Double] -> [Double] -> [Double]
varredTreeJac !t !(!cNames, !vNames) !consts !vars = concat $ ((<$>) . (<$>)) realToFrac $ jacobian (\cts -> [evalTree (zip cNames cts ++ zip vNames (realToFrac <$> vars)) (morphTreeConsts realToFrac t)]) (realToFrac <$> consts)

walkFail :: String -> Int -> Int -> a
walkFail s i n = error $ s ++ "; i = " ++ show i ++ "; n = " ++ show n

-- Better to place terminating optimizations at the top, obviously
simplifyTree :: (RealFloat a) => ExprTree a -> ExprTree a
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
simplifyTree (NodeBinary Div a b) | a == b = LeafConst 1.0
simplifyTree (NodeBinary f a b) | isConstNaN a' || isConstNaN b' = LeafConst $ 0.0 / 0.0
                                | a /= a' || b /= b' = simplifyTree n
                                | otherwise = n
    where (a', b') = join (***) simplifyTree (a, b)
          n = NodeBinary f a' b'
          isConstNaN (LeafConst c) = isNaN c
          isConstNaN _ = False
simplifyTree (NodeUnary f a) = NodeUnary f (simplifyTree a)
simplifyTree t = t

simplifyStab = simplifyTree

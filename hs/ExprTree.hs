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
import System.Random
import Data.List
import Debug.Trace

import SupportUtils
import Funcs
import Formattable

data Var = Var String
    deriving (Show, Eq, Ord)

data ExprTree a = NUn UnaryFunc (ExprTree a)
                | NBin BinaryFunc (ExprTree a) (ExprTree a)
                | LVar Var
                | LC a
    deriving (Show, Eq, Ord)

instance NFData a => NFData (ExprTree a)

class (Fractional a, Random a, Ord a, Eq a) => SuitableConst a
instance (Fractional a, Random a, Ord a, Eq a) => SuitableConst a

instance (Show a, Formattable a) => Formattable (ExprTree a) where
    pretty (LVar (Var x)) = x
    pretty (LC c) = pretty c
    pretty (NUn f t) = pretty f ++ " (" ++ pretty t ++ ")"
    pretty (NBin f l r) = "(" ++ pretty l ++ pretty f ++ pretty r ++ ")"

intLeaf = LC . fromInteger
realLeaf = LC

varLeaf = LVar . Var

unaryNode s = NUn <$> lookup s al 
    where al = [ ("sin", Sin), ("cos", Cos), ("log", Log), ("tan", Tan), ("atan", Atan), ("asin", Asin), ("acos", Acos) ]

binaryNode s = NBin <$> lookup s al
    where al = [ ("+", Plus), ("-", Minus), ("*", Mul), ("/", Div), ("^", Pow)]

randExprTree :: (RandomGen g, SuitableConst a) => [String] -> Int -> g -> (ExprTree a, g)
randExprTree vars cpx g = randExprTree' vars g (0, cpx)

randExprTree' :: forall a g. (RandomGen g, SuitableConst a) => [String] -> g -> (Int, Int) -> (ExprTree a, g)
randExprTree' vars g (dh, cpx) | dh /= 0 && thr dice 0.01 0.30 = (LC (dice * 50), g5)
                               | dh /= 0 && thr dice 0.30 0.30 = (LVar $ Var $ randElem vars g2, g5)
                               | dice <= 0.85 = (NBin
                                                    (randElem binaryOpsOnly g2)
                                                    (fst $ randExprTree' vars g3 (dh + 1, cpx))
                                                    (fst $ randExprTree' vars g4 (dh + 1, cpx)),
                                                 g5)
                               | otherwise = (NUn
                                                    (randElem unaryOpsOnly g2)
                                                    (fst $ randExprTree' vars g3 (dh + 1, cpx)),
                                                 g5)
    where dice = fst $ randomR (0, 1) g1 :: a
          (g0:g1:g2:g3:g4:g5:_) = take 6 (rndGens g)
          randElem xs g = xs !! fst (randomR (0, length xs - 1) g)
          (?) a b c | a = b
                    | otherwise = c
          thr dice t m = dice <= ((dh < cpx) ? t $ t / m)

numNodes :: ExprTree a -> Int
numNodes (LVar _) = 1
numNodes (LC _) = 1
numNodes (NUn _ t) = 1 + numNodes t
numNodes (NBin _ l r) = 1 + numNodes l + numNodes r

evalTree :: Floating a => [(String, a)] -> ExprTree a -> a
evalTree _ (LC c) = c
evalTree vars (LVar (Var v)) | Just c <- lookup v vars = c
                             | otherwise = error $ "Unknown var " ++ v
evalTree vars (NUn f t) = unaryOps f $ evalTree vars t
evalTree vars (NBin f l r) = binaryOps f (evalTree vars l) (evalTree vars r)

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
subTree' i n (LVar _) = walkFail "subTree: var node" i n
subTree' i n (LC _) = walkFail "subTree: const node" i n
subTree' i n (NBin _ l r) = atNodeBin subTree' i n (l, r)
subTree' i n (NUn _ t) = subTree' (i + 1) n t

repSubTree :: Int -> ExprTree a -> ExprTree a -> ExprTree a
repSubTree = repSubTree' 0

repSubTree' :: Int -> Int -> ExprTree a -> ExprTree a -> ExprTree a
repSubTree' i n r _ | i == n = r
repSubTree' i n r (LVar _) = walkFail "repSubTree: var node" i n
repSubTree' i n r (LC _) = walkFail "repSubTree: const node" i n
repSubTree' i n r (NBin f t1 t2) = NBin f t1' t2'
    where (t1', t2') = atNodeBin2 (\i n t -> repSubTree' i n r t) i n (t1, t2)
repSubTree' i n r (NUn f t) = NUn f (repSubTree' (i + 1) n r t)

varTreeConsts :: ExprTree a -> (ExprTree a, [(String, a)])
varTreeConsts = (\(x, y, z) -> (x, y)) . varTreeConsts' 0

varTreeConsts' :: Int -> ExprTree a -> (ExprTree a, [(String, a)], Int)
varTreeConsts' n t@(LVar _) = (t, [], n)
varTreeConsts' n t@(LC c) = (LVar (Var name), [(name, c)], n + 1)
    where name = '_' : show n
varTreeConsts' n (NUn f t) = (\(x, y, z) -> (NUn f x, y, z)) (varTreeConsts' n t)
varTreeConsts' n (NBin f l r) = (NBin f l' r', lrep ++ rrep, rpos)
    where (l', lrep, lpos) = varTreeConsts' n l
          (r', rrep, rpos) = varTreeConsts' lpos r

fixTreeVars :: [(String, a)] -> ExprTree a -> ExprTree a
fixTreeVars vals t@(LC _) = t
fixTreeVars vals t@(LVar (Var v)) | Just v' <- lookup v vals = LC v'
                                     | otherwise = t
fixTreeVars vals (NUn f t) = NUn f (fixTreeVars vals t)
fixTreeVars vals (NBin f l r) = NBin f (fixTreeVars vals l) (fixTreeVars vals r)

morphTreeConsts :: (a -> b) -> ExprTree a -> ExprTree b
morphTreeConsts c (LC ct) = LC $ c ct
morphTreeConsts _ (LVar v) = LVar v
morphTreeConsts c (NUn f t) = NUn f (morphTreeConsts c t)
morphTreeConsts c (NBin f l r) = NBin f (morphTreeConsts c l) (morphTreeConsts c r)

varredTreeJac :: (RealFloat a) => ExprTree a -> ([String], [String]) -> [a] -> [a] -> [a]
varredTreeJac t (cNames, vNames) consts vars = map (evalTree vals) parts
    where vals = zip (cNames ++ vNames) (consts ++ vars)
          parts = map (\v -> partDiff (Var v) t) cNames

walkFail :: String -> Int -> Int -> a
walkFail s i n = error $ s ++ "; i = " ++ show i ++ "; n = " ++ show n

isSameTreeStruct :: ExprTree a -> ExprTree a -> Bool
isSameTreeStruct (LC _) (LC _) = True
isSameTreeStruct (LVar v1) (LVar v2) = v1 == v2
isSameTreeStruct (NUn f1 t1) (NUn f2 t2) = f1 == f2 && isSameTreeStruct t1 t2
isSameTreeStruct (NBin f1 l1 r1) (NBin f2 l2 r2) = f1 == f2 && isSameTreeStruct l1 l2 && isSameTreeStruct r1 r2
isSameTreeStruct _ _ = False

ufDiff :: (Fractional a, Num a) => UnaryFunc -> ExprTree a -> ExprTree a
ufDiff Sin t = NUn Cos t
ufDiff Cos t = NBin Mul (LC (-1)) (NUn Sin t)
ufDiff Log t = NBin Div (LC 1) t
ufDiff Tan t = NBin Div (LC 1) (NBin Pow (NUn Cos t) (LC 2))
ufDiff Asin t = NBin Div (LC 1) (NBin Pow (NBin Minus (LC 1) (NBin Pow t (LC 2))) (LC 0.5))
ufDiff Acos t = NBin Div (LC (-1)) (NBin Pow (NBin Minus (LC 1) (NBin Pow t (LC 2))) (LC 0.5))
ufDiff Atan t = NBin Div (LC 1) (NBin Plus (LC 1) (NBin Pow t (LC 2)))

bfDiff :: (Floating a, Real a) => BinaryFunc -> (ExprTree a, ExprTree a) -> (ExprTree a, ExprTree a)-> ExprTree a
bfDiff Plus _ (dl, dr) = NBin Plus dl dr
bfDiff Minus _ (dl, dr) = NBin Minus dl dr
bfDiff Mul (l, r) (dl, dr) = NBin Plus (NBin Mul l dr) (NBin Mul dl r)
bfDiff Div (l, r) (dl, dr) = NBin Div t (NBin Pow r (LC 2))
    where t = NBin Minus (NBin Mul dl r) (NBin Mul l dr)
bfDiff Pow (f, g) (df, dg) = NBin Mul lt rt
    where lt = NBin Plus (NBin Mul dg (NUn Log f)) (NBin Mul g (NBin Div df f))
          rt = NBin Pow (LC (exp 1)) (NBin Mul g (NUn Log f))
-- ^^^ here we used that (f^g)' = (e^(g ln f))' = (g ln f)' e^(g ln f)
-- (g ln f)' is named lt, the rest is rt

partDiff :: (RealFloat a) => Var -> ExprTree a -> ExprTree a
partDiff _ (LC _) = LC 0
partDiff v (LVar lv) | lv == v = LC 1
                     | otherwise = LC 0
partDiff v (NUn f t) = NBin Mul (ufDiff f t) (partDiff v t)
partDiff v (NBin f l r) = bfDiff f (l, r) (partDiff v l, partDiff v r)

-- Better to place terminating optimizations at the top, obviously
simplifyTree :: (RealFloat a) => ExprTree a -> ExprTree a
simplifyTree (NBin Plus (LC 0.0) a) = a
simplifyTree (NBin Pow _ (LC 0.0)) = LC 1.0
simplifyTree (NBin Pow l@(LC 1.0) _) = l
simplifyTree (NBin Mul _ l@(LC 0.0)) = l
simplifyTree (NUn a (LC x)) = LC $ unaryOps a x
simplifyTree (NBin a (LC x) (LC y)) = LC $ binaryOps a x y
simplifyTree (NBin f a r@(LC c)) | comm $ binFInf f = simplifyTree $ NBin f (simplifyTree r) a
simplifyTree (NBin f l r) | numNodes l > numNodes r && (comm $ binFInf f) = simplifyTree $ NBin f r l
simplifyTree (NBin f1 (LC lc) (NBin f2 (LC rc) t)) | f1 == f2 && (assoc $ binFInf f1) = simplifyTree $ NBin f1 (LC $ binaryOps f1 lc rc) (simplifyTree t)
simplifyTree (NBin Pow a (LC 1.0)) = simplifyTree a
simplifyTree (NBin Mul (LC 1.0) a) = simplifyTree a
simplifyTree (NBin Minus a b) | a == b = LC 0.0
simplifyTree (NBin Minus a (LC b)) | b < 0 = NBin Plus a (LC (-b))
simplifyTree (NBin Div a b) | a == b = LC 1.0
simplifyTree (NBin f a b) | isConstNaN a' || isConstNaN b' = LC $ 0.0 / 0.0
                          | a /= a' || b /= b' = simplifyTree n
                          | otherwise = n
    where (a', b') = join (***) simplifyTree (a, b)
          n = NBin f a' b'
          isConstNaN (LC c) = isNaN c
          isConstNaN _ = False
simplifyTree (NUn f a) = NUn f (simplifyTree a)
simplifyTree t = t

simplifyStab = simplifyTree

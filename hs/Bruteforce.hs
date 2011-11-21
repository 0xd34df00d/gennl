{-# LANGUAGE NamedFieldPuns #-}

module Bruteforce
    where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import ExprTree
import Funcs

class Composable a where
    createVar :: String -> a
    compose1 :: UnaryFunc -> a -> a
    compose2 :: BinaryFunc -> (a, a) -> a

data AnyFunc = UF { uf :: UnaryFunc } |
               BF { bf :: BinaryFunc }

data BfConfig = BfConfig {
        unaryFuncs :: [UnaryFunc],
        binaryFuncs :: [BinaryFunc],
        vars :: [String]
    }
    deriving (Show)

data BfState a = BfState {
        cfg :: BfConfig,
        iter :: Int,
        ppl :: Set a,
        iters :: Map a Int,
        gens :: Map Int a
    }
    deriving (Show)

instance Composable (ExprTree a) where
    createVar = LVar . Var
    compose1 = NUn
    compose2 = uncurry . NBin

combine :: Composable a => AnyFunc -> [a] -> [a]
combine (UF f) ppls = [ compose1 f t | t <- ppls ]
combine (BF f) ppls = [ compose2 f (t1, t2) | t1 <- ppls, t2 <- ppls ]

defBf :: BfConfig
defBf = BfConfig unaryOpsOnly binaryOpsOnly ["x", "y", "z"]

initBf :: (Ord a, Composable a) => BfConfig -> BfState a
initBf cfg = BfState cfg 0 (S.fromList p) (M.fromList $ zip p (repeat 0)) (M.fromList $ zip (repeat 0) p)
   where p = map createVar (vars cfg) 

stepBf :: (Ord a, Composable a) => BfState a -> BfState a
stepBf st@(BfState c i ppl iters gens) = st { iter = i', ppl = ppl `S.union` ps }
    where p = [ compose1 f t | f <- unaryFuncs c, t <- ppl' ] ++ [ compose2 f (t1, t2) | f <- binaryFuncs c, t1 <- ppl', t2 <- ppl' ]
          ps = S.fromList p
          ppl' = S.toList ppl
          news = S.difference ps (S.intersection ps ppl)
          (iters', gens') = S.fold (\p (iters, gens) -> (M.insert p i iters, M.insert i p gens)) (iters, gens) news
          i' = i + 1

runBf :: (Composable a, Ord a) => BfState a -> [BfState a]
runBf = iterate stepBf

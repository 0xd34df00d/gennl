{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Genetic
    where

import Control.Monad.State
import Control.Arrow
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Functor ((<$>))
import Numeric.FAD
import Debug.Trace
import Data.Ord (comparing)
import Random

import ExprTree
import SupportUtils
import Formattable
import Funcs
import qualified LevMar

data GAConfig a = GAConfig {
        unaryOpsPool :: [UnaryFunc],
        binaryOpsPool :: [BinaryFunc],
        vars :: [String],
        testSet :: [([ComputeRes a], ComputeRes a)],                     -- The order of doubles should be the same as in vars.
        rndCpx :: Int,
        optNum :: Int,
        stopF :: [a] -> Int -> ComputeRes a -> Bool
    }

data RandomGen g => GAState g a = GAState {
        cfg :: GAConfig a,
        randGen :: g,
        iter :: Int,
        ppl :: [a],
        fits :: [(a, ComputeRes a)],
        optimized :: [a]
    }

class (Eq a, Show a, Formattable a, NFData a, NFData (ComputeRes a), Ord (ComputeRes a), RealFloat (ComputeRes a), Formattable (ComputeRes a)) => GAble a where
    type ComputeRes a :: *
    mutate :: RandomGen g => GAState g a -> g -> a -> a
    crossover :: RandomGen g => GAState g a -> g -> (a, a) -> (a, a)
    compute :: [(String, ComputeRes a)] -> a -> ComputeRes a
    randGAInst :: RandomGen g => [String] -> Int -> g -> (a, g)
    variateConsts :: a -> (a, [(String, ComputeRes a)])
    fixVars :: [(String, ComputeRes a)] -> a -> a
    jacForConsts :: a -> ([String], [String]) -> [ComputeRes a] -> [ComputeRes a] -> [ComputeRes a]
    complexity :: a -> Double
    complexity _ = 1.0
    res2double :: ComputeRes a -> Double
    res2double = realToFrac
    double2res :: Double -> ComputeRes a
    double2res = realToFrac
    simplify :: a -> a
    simplify = id
    isSameStruct :: a -> a -> Bool
    isSameStruct = (==)

defConfig :: (GAble a) => GAConfig a
defConfig = GAConfig
                (map fst unaryOps)
                (map fst binaryOps)
                []
                []
                4
                50
                (\_ its maxF -> its > 50 || (its > 0 && maxF > 0.95))

initGA :: (RandomGen g, GAble a) => GAConfig a -> g -> GAState g a
initGA c g = GAState c g 0 [] [] []

initPpl :: (RandomGen g, GAble a) => Int -> GAState g a -> GAState g a
initPpl n st = st { ppl = map simplify $ take n $ unfoldr (Just . randGAInst (vars c) (rndCpx c)) g1, randGen = g2 }
    where (g1, g2) = split $ randGen st
          c = cfg st

iterateGA :: (RandomGen g, GAble a) => GAState g a -> GAState g a
iterateGA = execState chain
    where chain = optimizePplConsts >>
                    reassess >>
                    tickGA >>
                    mutateSome >>
                    crossoverSome >>
                    reassess
          reassess = assessPpl >>
                    clean nanF >>
                    sortPpl >>
                    clean sameF

type MGState g a = State (GAState g a) ()

tickGA :: (GAble a, RandomGen g) => MGState g a
tickGA = get >>= (\st -> (iter st + 1, length $ ppl st, map pretty (drop (length (fits st) - 5) (fits st))) `traceShow` put $ st { iter = iter st + 1 } )

optimizePplConsts :: (GAble a, RandomGen g) => MGState g a
optimizePplConsts = do
    st <- get
    let opted = optimized st
    let unopt = ppl st \\ opted
    let !opt = parListChunk (length unopt `div` 16) rdeepseq `withStrategy` map (optimizeConsts (cfg st)) unopt
    put st { ppl = opted ++ opt, optimized = opted ++ opt, fits = [] } --filter (\x -> fst x `elem` opted) (fits st) }

runOpt :: (GAble a) => [String] -> [([ComputeRes a], ComputeRes a)] -> [(String, ComputeRes a)] -> a -> [ComputeRes a]
runOpt vars dat cv a = LevMar.fitModel (gaModel (a, cvNames ++ vars)) j (map (\(a, b) -> (b, a)) dat) (map snd cv)
    where cvNames = fst <$> cv
          j (c, v) = jacForConsts a (cvNames, vars) c v

gaModel :: (GAble a) => (a, [String]) -> ([ComputeRes a], [ComputeRes a]) -> ComputeRes a
gaModel !(!a, !names) !(!consts, !vars) = compute (zip names (consts ++ vars)) a
--{-# SPECIALIZE gaModel :: (ExprTree Double, [String]) -> ([Double], [Double]) -> Double #-}

optimizeConsts :: (GAble a) => GAConfig a -> a -> a
optimizeConsts cfg a = simplify $ fixVars (zip (map fst cv) res) varred
    where (varred, cv) = variateConsts a
          res = runOpt (vars cfg) (testSet cfg) cv varred

assessPpl :: (GAble a, RandomGen g) => MGState g a
assessPpl = do
        st <- get
        let ppls = ppl st
        let !as = parListChunk (length ppls `div` 16) rdeepseq `withStrategy` map (getFit st) ppls
        put st { fits = zip ppls as }
    where getFit st a | Just x <- lookup a (fits st) = x
                      | otherwise = getChromoFit a st

getChromoFit :: (RandomGen g, GAble a) => a -> GAState g a -> ComputeRes a
getChromoFit a st = 1 / (sum xs + 1) * cpxPen
    where xs = map f (testSet c)
          f smp = sqrt ((snd smp - compute (zip (vars c) (fst smp)) a) ** 2)
          c = cfg st
          cpxPen = realToFrac $ 0.95 + 0.05 / (1 + (exp 1) ** (complexity a - 10))

clean :: (GAble a, RandomGen g) => (GAState g a -> [a] -> [a]) -> MGState g a
clean cleaner = do
        st <- get
        let ppls' = cleaner st (ppl st)
        let ppls = drop (diff ppls' st) ppls'
        let rem = ppl st \\ ppls
        put st { ppl = ppls, fits = filter (not . (`elem` rem) . fst) (fits st) }
            where diff p st = length p - optNum (cfg st)

nanF :: (RandomGen g, GAble a) => GAState g a -> [a] -> [a]
nanF st = {-("FITS", map snd $ fits st) `traceShow`-} filter (\a -> not $ isNaN $ fromMaybe (0/0) (liftM snd $ find (isSameStruct a . fst) (fits st)))

sameF :: (RandomGen g, GAble a) => GAState g a -> [a] -> [a]
sameF st = reverse . delConseq (\a b -> lookup a fs == lookup b fs || isSameStruct a b) . reverse
    where fs = fits st

delConseq :: (a -> a -> Bool) -> [a] -> [a]
delConseq _ [] = []
delConseq p (x:xs) = reverse $ snd (foldl' step (x, [x]) xs)
    where step l@(x', r) x | p x x' = l
                           | otherwise = (x, x : r)

sortPpl :: (RandomGen g, GAble a) => MGState g a
sortPpl = do
        st <- get
        put $ st { ppl = sortBy (cmp $ fits st) (ppl st), fits = sortBy (comparing snd) (fits st) }
            where cmp fs = comparing (\x -> fromMaybe (0/0) (lookup x fs))

mutateSome :: (RandomGen g, GAble a) => MGState g a
mutateSome = do
        st <- get
        let ppls = ppl st
        let toTake = length ppls `div` 3
        let gs = rndGens $ randGen st
        let news = zipWith (mutate st) gs (take toTake ppls)
        put st { ppl = news ++ ppls, randGen = gs !! toTake }

crossoverSome :: (RandomGen g, GAble a) => MGState g a
crossoverSome = do
        st <- get
        unless (1 `elem` map snd (fits st)) $ do
            let ppls = ppl st
            let l = length ppls
            let rest = lastN (min 7 (l `div` 3)) ppls
            let gs = rndGens $ randGen st
            let pairs = ns [ (m1, m2) | m1 <- rest, m2 <- takeWhile (/= m1) rest ]
            let news = ns $ withStrategy rseq $ zipWith (crossover st) gs pairs
            let nl = length news
            put st { ppl = ppls ++ concatMap (\(x, y) -> [x, y]) news, randGen = gs !! nl }
                where ns = filter (uncurry (/=))
                      lastN n xs = drop (length xs - n) xs

runGA :: (RandomGen g, GAble a) => GAState g a -> (a, ComputeRes a, GAState g a)
runGA st | stopF (cfg st') (ppl st') (iter st') maxFitness = (best, maxFitness, st')
         | otherwise = runGA st'
    where st' = iterateGA st
          (best, maxFitness) = maximumBy (comparing snd) (filter (not . isNaN . snd) (fits st'))

instance (SuitableConst a, Num a, Real a, NFData a, Floating a, Formattable a, RealFloat a) => GAble (ExprTree a) where
    type ComputeRes (ExprTree a) = a
    mutate = mutateTree
    crossover = coTrees
    compute = evalTree
    randGAInst = randExprTree
    complexity = fromIntegral . numNodes
    variateConsts = varTreeConsts
    fixVars = fixTreeVars
    jacForConsts = varredTreeJac
    simplify = simplifyStab
    isSameStruct = isSameTreeStruct

mutateTree :: (RandomGen g, Random a, RealFloat a) => GAState g (ExprTree a) -> g -> ExprTree a -> ExprTree a
mutateTree st g t = simplifyStab $ mutateTree' st g1 t (fst $ randomR (0, numNodes t - 1) g2) 0
    where (g1, g2) = split g

mutateTree' :: (RandomGen g, Random a, Num a) => GAState g (ExprTree a) -> g -> ExprTree a -> Int -> Int -> ExprTree a
mutateTree' st g (NBin f l r) n i | n == i = NBin (mutGene f (binaryOpsPool $ cfg st) g) l r
                                        | nl + i >= n = NBin f (mutateTree' st g l n (i + 1)) r
                                        | otherwise = NBin f l (mutateTree' st g r n (i + nl + 1))
                                            where nl = numNodes l
mutateTree' st g (NUn f t) n i | n == i = NUn (mutGene f (unaryOpsPool $ cfg st) g) t
                                     | otherwise = NUn f (mutateTree' st g t n (i + 1))
mutateTree' st g l@(LVar (Var v)) n i | n == i = LVar $ Var $ mutGene v (vars $ cfg st) g
                                         | otherwise = error $ "WTF? Var node, i = " ++ show i ++ "; n = " ++ show n
mutateTree' st g l@(LC _) n i | n == i = LC $ fst (random g) * 100
                                     | otherwise = error $ "WTF? Const node, i = " ++ show i ++ "; n = " ++ show n

mutGene :: (Eq a, RandomGen g) => a -> [a] -> g -> a
mutGene ex vars g = fi !! fst (randomR (0, length fi - 1) g)
    where fi = filter (/= ex) vars

coTrees :: (RandomGen g, Show a, RealFloat a) => GAState g (ExprTree a) -> g -> (ExprTree a, ExprTree a) -> (ExprTree a, ExprTree a)
coTrees st g (t1, t2) = {-("CV", pretty t1, pretty t2, "NEW:", pretty t1', pretty t2') `traceShow` -}(t1', t2')
    where gs = rndGens g
          p1 = getP (gs !! 0) t1
          p2 = getP (gs !! 1) t2
          t1' = swap (t1, p1) (t2, p2)
          t2' = swap (t2, p2) (t1, p1)
          getP g t = fst $ randomR (0, numNodes t - 1) g
          swap (t1, p1) (t2, p2) = simplifyStab $ repSubTree p1 (subTree p2 t2) t1

-- Utility stuff
randElem :: [a] -> Double -> a
randElem xs r = xs !! floor (r * fromIntegral (length xs))

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (x:xs) 0 x' = x' : xs
replaceElem (x:xs) n x' = x : replaceElem xs (n - 1) x'

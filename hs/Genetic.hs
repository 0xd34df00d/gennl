module Genetic
    where

import Control.Monad.State
import Control.Arrow
import Control.Parallel.Strategies
import Data.Packed.Matrix
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Ord (comparing)
import Random
import ExprTree
import ExprIncidenceMatrix
import SupportUtils
import Formattable

type TestSample = ([Double], Double)

data GAConfig a = GAConfig {
        unaryOpsPool :: [UnaryFunc],
        binaryOpsPool :: [BinaryFunc],
        vars :: [String],
        testSet :: [TestSample],                     -- The order of doubles should be the same as in vars.
        rndCpx :: Int,
        optNum :: Int,
        stopF :: [a] -> Int -> Double -> Bool
    }

data RandomGen g => GAState g a = GAState {
        cfg :: GAConfig a,
        randGen :: g,
        iter :: Int,
        ppl :: [a],
        fits :: [(a, Double)]
    }

class (Eq a, Show a, Formattable a) => GAble a where
    mutate :: RandomGen g => GAState g a -> g -> a -> a
    crossover :: RandomGen g => GAState g a -> g -> (a, a) -> (a, a)
    compute :: [(String, Double)] -> a -> Double
    randGAInst :: RandomGen g => [String] -> Int -> g -> (a, g)
    complexity :: a -> Double
    complexity _ = 1.0

defConfig :: (GAble a) => GAConfig a
defConfig = GAConfig
                (map fst unaryOps)
                (map fst binaryOps)
                []
                []
                7
                200
                (\_ its maxF -> its > 100 || maxF > 0.95)

initGA :: (RandomGen g, GAble a) => GAConfig a -> g -> GAState g a
initGA c g = GAState c g 0 [] []

initPpl :: (RandomGen g, GAble a) => Int -> GAState g a -> GAState g a
initPpl n st = st { ppl = take n $ unfoldr (Just . randGAInst (vars c) (rndCpx c)) g1, randGen = g2 }
    where (g1, g2) = split $ randGen st
          c = cfg st

iterateGA :: (RandomGen g, GAble a) => GAState g a -> GAState g a
iterateGA = execState chain
    where chain = assessPpl >>
                  sortPpl >>
                  cleanBad >>
                  cleanupFits >>
                  tickGA >>
                  mutateSome >>
                  assessPpl >>
                  sortPpl >>
                  crossoverSome >>
                  assessPpl >>
                  sortPpl

type MGState g a = State (GAState g a) ()

tickGA :: (GAble a, RandomGen g) => MGState g a
tickGA = get >>= (\st -> (iter st + 1, length $ ppl st, map pretty (drop (length (fits st) - 5) (fits st))) `traceShow` put $ st { iter = iter st + 1 } )

assessPpl :: (GAble a, RandomGen g) => MGState g a
assessPpl = do
        st <- get
        let ppls = ppl st
        put st { fits = zip ppls (withStrategy (parListChunk 5 rdeepseq) $ map (getFit st) ppls) }
    where getFit st a | Just x <- lookup a (fits st) = x
                      | otherwise = getChromoFit a st

getChromoFit :: (RandomGen g, GAble a) => a -> GAState g a -> Double
getChromoFit a st = 1 / (sum xs + 1)
    where xs = map f (testSet c)
          f smp = sqrt (abs (snd smp - compute (zip (vars c) (fst smp)) a))
          c = cfg st

cleanBad :: (GAble a, RandomGen g) => MGState g a
cleanBad = do
        st <- get
        let ppls' = ppl st
        let fs = fits st
        let ppls = delConseq (\a b -> lookup a fs == lookup b fs) $ filter (\a -> not $ isNaN $ fromMaybe (0/0) (lookup a fs)) ppls'
        put st { ppl = drop (diff ppls st) ppls }
            where diff p st = length p - optNum (cfg st)

delConseq :: (a -> a -> Bool) -> [a] -> [a]
delConseq _ [] = []
delConseq p (x:xs) = reverse $ snd (foldl' step (x, [x]) xs)
    where step l@(x', r) x | p x x' = l
                           | otherwise = (x, x : r)

sortPpl :: (RandomGen g, GAble a) => MGState g a
sortPpl = get >>= (\st -> put $ st { ppl = map fst (filter (isNaN . snd) (fits st) ++ sortBy (comparing snd) (filter (not . isNaN . snd) (fits st))) } )

cleanupFits :: (RandomGen g, GAble a) => MGState g a
cleanupFits = get >>=
    (\st -> when (length (ppl st) /= length (fits st)) $ put $ st { fits = filter ((`elem` ppl st) . fst) (fits st) } )

mutateSome :: (RandomGen g, GAble a) => MGState g a
mutateSome = do
        st <- get
        let ppls = ppl st
        let toTake = length ppls `div` 5
        let gs = rndGens $ randGen st
        let news = zipWith (mutate st) gs (take toTake ppls)
        put st { ppl = news ++ ppls, randGen = gs !! toTake }

crossoverSome :: (RandomGen g, GAble a) => MGState g a
crossoverSome = do
        st <- get
        unless (1 `elem` map snd (fits st)) $ do
            let ppls = ppl st
            let l = length ppls
            let rest = lastN (min 7 (l `div` 10)) ppls
            let gs = rndGens $ randGen st
            let pairs = ns [ (m1, m2) | m1 <- rest, m2 <- rest ]
            let news = ns $ withStrategy rseq $ zipWith (crossover st) gs pairs
            let nl = length news
            put st { ppl = ppls ++ concatMap (\(x, y) -> [x, y]) news, randGen = gs !! nl }
                where ns = filter (uncurry (/=))
                      lastN n xs = drop (length xs - n) xs

runGA :: (RandomGen g, GAble a) => GAState g a -> (a, Double, GAState g a)
runGA = runGA' . iterateGA

runGA' :: (RandomGen g, GAble a) => GAState g a -> (a, Double, GAState g a)
runGA' st = if stopF (cfg st) (ppl st) (iter st) maxFitness
            then (best, maxFitness, st)
            else runGA' $ iterateGA st
    where (best, maxFitness) = maximumBy (comparing snd) (filter (not . isNaN . snd) (fits st))

instance GAble IncMatrix where
    mutate st g m = m'
        where (t1:t2:t3:_, gen) = nRands g 3
              opIdx = floor $ fromIntegral (rows (numMat m) + 1) * t1
              newOp = case ops m !! opIdx of
                    LeafCNode _ -> leafMutate (t2, t3) (cfg st)
                    LeafTNode _ -> leafMutate (t2, t3) (cfg st)
                    UnNode f -> unaryMutate t2 (cfg st)
                    BinNode f -> binaryMutate t2 (cfg st)
              m' = m { ops = replaceElem (ops m) opIdx newOp }
              leafMutate (t1, t2) c | t1 >= 0.5 = LeafCNode $ t2 * 100
                                    | otherwise = LeafTNode $ randElem (vars c) t2
              unaryMutate t1 c = UnNode $ randElem (unaryOpsPool c) t1
              binaryMutate t1 c = BinNode $ randElem (binaryOpsPool c) t1
    crossover st g (m1, m2) = (m1', m2')
        where gs = rndGens g
              getP m g = fst $ randomR (1, cols (numMat m) - 1) g
              p1 = getP m1 (head gs)
              p2 = getP m2 (gs !! 1)
              m1' = swap'' (m1, p1) (m2, p2)
              m2' = swap'' (m2, p2) (m1, p1)
              swap'' (m1, p1) (m2, p2) = replaceSubMat p1 (subTreeMat p2 m2) m1
                    where m1s = cols $ numMat m1
                          m2s = cols $ numMat m2
    compute = evalMatrix
    randGAInst = randIncMatrix

instance GAble ExprTree where
    mutate = mutateTree
    crossover = coTrees
    compute = evalTree
    randGAInst = randExprTree
    complexity = fromIntegral . numNodes

mutateTree :: (RandomGen g) => GAState g ExprTree -> g -> ExprTree -> ExprTree
mutateTree st g t = mutateTree' st g1 t (fst $ randomR (0, numNodes t - 1) g2) 0
    where (g1, g2) = split g

mutateTree' :: (RandomGen g) => GAState g ExprTree -> g -> ExprTree -> Int -> Int -> ExprTree
mutateTree' st g (NodeBinary f l r) n i | n == i = NodeBinary (mutGene f (binaryOpsPool $ cfg st) g) l r
                                        | nl + i >= n = NodeBinary f (mutateTree' st g l n (i + 1)) r
                                        | otherwise = NodeBinary f l (mutateTree' st g r n (i + nl + 1))
                                            where nl = numNodes l
mutateTree' st g (NodeUnary f t) n i | n == i = NodeUnary (mutGene f (unaryOpsPool $ cfg st) g) t
                                     | otherwise = NodeUnary f (mutateTree' st g t n (i + 1))
mutateTree' st g l@(LeafVar (Var v)) n i | n == i = LeafVar $ Var $ mutGene v (vars $ cfg st) g
                                         | otherwise = error $ "WTF? Var node, i = " ++ show i ++ "; n = " ++ show n
mutateTree' st g l@(LeafConst _) n i | n == i = LeafConst $ fst (random g) * 100
                                     | otherwise = error $ "WTF? Const node, i = " ++ show i ++ "; n = " ++ show n

mutGene :: (Eq a, RandomGen g) => a -> [a] -> g -> a
mutGene ex vars g = fi !! fst (randomR (0, length fi - 1) g)
    where fi = filter (/= ex) vars

coTrees :: (RandomGen g) => GAState g ExprTree -> g -> (ExprTree, ExprTree) -> (ExprTree, ExprTree)
coTrees st g (t1, t2) = (t1', t2')
    where gs = rndGens g
          p1 = getP (gs !! 0) t1
          p2 = getP (gs !! 1) t2
          t1' = swap (t1, p1) (t2, p2)
          t2' = swap (t2, p2) (t1, p1)
          getP g t = fst $ randomR (0, numNodes t - 1) g
          swap (t1, p1) (t2, p2) = repSubTree p1 (subTree p2 t2) t1

-- Utility stuff
randElem :: [a] -> Double -> a
randElem xs r = xs !! floor (r * fromIntegral (length xs))

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (x:xs) 0 x' = x' : xs
replaceElem (x:xs) n x' = x : replaceElem xs (n - 1) x'

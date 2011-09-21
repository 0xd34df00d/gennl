module Genetic
    where

import Control.Monad.State
import Control.Arrow
import Control.Parallel.Strategies
import Data.Packed.Matrix
import Data.List
import Debug.Trace
import Data.Ord (comparing)
import Random
import ExprTree
import ExprIncidenceMatrix
import SupportUtils

type TestSample = ([Double], Double)

data GAConfig a = GAConfig {
        unaryOpsPool :: [UnaryFunc],
        binaryOpsPool :: [BinaryFunc],
        vars :: [String],
        testSet :: [TestSample],                     -- The order of doubles should be the same as in vars.
        rndCpx :: Int,
        stopF :: [a] -> Int -> Double -> Bool
    }

data RandomGen g => GAState g a = GAState {
        cfg :: GAConfig a,
        randGen :: g,
        iter :: Int,
        ppl :: [a],
        fits :: [(a, Double)]
    }

class (Eq a, Show a) => GAble a where
    mutate :: RandomGen g => GAState g a -> g -> a -> a
    crossover :: RandomGen g => GAState g a -> g -> (a, a) -> (a, a)
    compute :: [(String, Double)] -> a -> Double
    randGAInst :: RandomGen g => [String] -> Int -> g -> (a, g)

defConfig :: (GAble a) => GAConfig a
defConfig = GAConfig
                (map fst unaryOps)
                (map fst binaryOps)
                []
                []
                7
                (\_ its maxF -> its > 100 || maxF > 0.95)

initGA :: (RandomGen g, GAble a) => GAConfig a -> g -> GAState g a
initGA c g = GAState c g 0 [] []

initPpl :: (RandomGen g, GAble a) => Int -> GAState g a -> GAState g a
initPpl n st = st { ppl = take n $ unfoldr (Just . randGAInst (vars c) (rndCpx c)) g1, randGen = g2 }
    where (g1, g2) = split $ randGen st
          c = cfg st

iterateGA :: (RandomGen g, GAble a) => GAState g a -> GAState g a
iterateGA = execState chain
    where chain = tickGA >>
                    assessPpl >>
                    sortPpl >>
                    mutateSome >>
                    cleanupFits >>
                    assessPpl >>
                    sortPpl >>
                    crossoverSome >>
                    assessPpl >>
                    sortPpl

type MGState g a = State (GAState g a) ()

tickGA :: RandomGen g => MGState g a
tickGA = get >>= (\st -> (show $ iter st + 1) `trace` put $ st { iter = iter st + 1 } )

assessPpl :: (GAble a, RandomGen g) => MGState g a
assessPpl = do
        st <- get
        let ppls = ppl st
        put st { fits = zip ppls (parMap rdeepseq (getFit st) ppls) }
    where getFit st a | Just x <- lookup a (fits st) = x
                      | otherwise = getChromoFit a st

getChromoFit :: (RandomGen g, GAble a) => a -> GAState g a -> Double
getChromoFit a st = 1 / (sum xs + 1)
    where xs = map f (testSet c)
          f smp = abs (snd smp - compute (zip (vars c) (fst smp)) a)
          c = cfg st

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
        put st { ppl = news ++ drop toTake ppls, randGen = gs !! toTake }

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
            put st { ppl = drop (nl * 2) ppls ++ concatMap (\(x, y) -> [x, y]) news, randGen = gs !! nl }
                where ns = filter (uncurry (/=))
                      lastN n xs = drop (length xs - n) xs

runGA :: (RandomGen g, GAble a) => GAState g a -> (a, Double, GAState g a)
runGA = runGA' . iterateGA

runGA' :: (RandomGen g, GAble a) => GAState g a -> (a, Double, GAState g a)
runGA' st = if stopF (cfg st) (ppl st) (iter st) maxFitness
            then (best, maxFitness, st)
            else runGA $ iterateGA st
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
              getP m g = fst $ randomR (1, (cols $ numMat m) - 1) g
              p1 = getP m1 (head gs)
              p2 = getP m2 (gs !! 1)
              m1' = swap'' (m1, p1) (m2, p2)
              m2' = swap'' (m2, p2) (m1, p1)
              swap'' (m1, p1) (m2, p2) = replaceSubMat p1 (subTreeMat p2 m2) m1
                    where m1s = cols $ numMat m1
                          m2s = cols $ numMat m2
    compute = evalMatrix
    randGAInst = randIncMatrix

-- Utility stuff
randElem :: [a] -> Double -> a
randElem xs r = xs !! floor (r * fromIntegral (length xs))

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (x:xs) 0 x' = x' : xs
replaceElem (x:xs) n x' = x : replaceElem xs (n - 1) x'

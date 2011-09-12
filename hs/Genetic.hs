module Genetic
    where

import Control.Monad.State
import Control.Arrow
import Data.Packed.Matrix
import Data.List
import Random
import ExprTree
import ExprIncidenceMatrix

type TestSample = ([Double], Double)

data GAConfig a = GAConfig {
        unaryOpsPool :: [UnaryFunc],
        binaryOpsPool :: [BinaryFunc],
        vars :: [String],
        testSet :: [TestSample],                     -- The order of doubles should be the same as in vars.
        stopF :: [a] -> Int -> Double -> Bool
    }

data RandomGen g => GAState g a = GAState {
        config :: GAConfig a,
        randGen :: g,
        iter :: Int,
        population :: [a],
        knownFitnesses :: [(a, Double)]
    }

class GAble a where
    mutate :: RandomGen g => GAState g a -> a -> (a, GAState g a)
    crossover :: RandomGen g => GAState g a -> (a, a) -> (a, a, GAState g a)
    compute :: [(String, Double)] -> a -> Double

defConfig :: (GAble a) => GAConfig a
defConfig = GAConfig
                (map fst unaryOps)
                (map fst binaryOps)
                []
                []
                (\_ its maxF -> its > 1000 || maxF > 0.95)

initGA :: (RandomGen g, GAble a) => GAConfig a -> g -> [a] -> GAState g a
initGA c g as = GAState c g 0 as []

iterateGA :: (RandomGen g, GAble a) => GAState g a -> GAState g a
iterateGA = execState chain
    where chain = tickGA

tickGA :: RandomGen g => State (GAState g a) ()
tickGA = get >>= (\st -> put $ st { iter = iter st + 1 } )

instance GAble IncMatrix where
    mutate st m = (m', st')
        where (t1:t2:t3:_, gen) = nRands (randGen st) 3
              opIdx = floor $ fromIntegral (rows (numMat m) + 1) * t1
              newOp = case ops m !! opIdx of
                    LeafCNode _ -> leafMutate (t2, t3) (config st)
                    LeafTNode _ -> leafMutate (t2, t3) (config st)
                    UnNode f -> unaryMutate t2 (config st)
                    BinNode f -> binaryMutate t2 (config st)
              m' = m { ops = replaceElem (ops m) opIdx newOp }
              st' = st { randGen = gen }
              leafMutate (t1, t2) c | t1 >= 0.5 = LeafCNode $ t2 * 100
                                    | otherwise = LeafTNode $ randElem (vars c) t2
              unaryMutate t1 c = UnNode $ randElem (unaryOpsPool c) t1
              binaryMutate t1 c = BinNode $ randElem (binaryOpsPool c) t1
    crossover st (m1, m2) = (m1', m2', st')
        where (p1:p2:_, gen) = nRands (randGen st) 2
              m1' = swap'' (m1, p1) (m2, p2)
              m2' = swap'' (m2, p2) (m1, p1)
              st' = st { randGen = gen }
              swap'' (m1, p1) (m2, p2) = replaceSubMat (p1 `mod` m1s) (subTreeMat (p2 `mod` m2s) m2) m1
                    where m1s = cols $ numMat m1
                          m2s = cols $ numMat m2
    compute = evalMatrix

-- Utility stuff
nRands :: (RandomGen g, Random a) => g -> Int -> ([a], g)
nRands g n = (take n $ unfoldr (Just . random) g1, g2)
    where (g1, g2) = split g

randElem :: [a] -> Double -> a
randElem xs r = xs !! floor (r * fromIntegral (length xs))

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (x:xs) 0 x' = x' : xs
replaceElem (x:xs) n x' = x : replaceElem xs (n - 1) x'

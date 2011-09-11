module Genetic
    where

import Control.Monad.State
import Control.Arrow
import Data.Packed.Matrix
import Data.List
import Random
import ExprTree
import ExprIncidenceMatrix

data GAConfig a = GAConfig {
        unaryOpsPool :: [UnaryFunc],
        binaryOpsPool :: [BinaryFunc],
        vars :: [String],
        fitF :: a -> Double,
        stopF :: [a] -> Int -> Double -> Bool,
        leaveF :: [(Double, a)] -> [(Double, a)] -> [a]
    }

data GAState a = GAState {
        config :: GAConfig a,
        randGen :: StdGen,
        iter :: Int,
        population :: [a]
    }

class GAble a where
    mutate :: GAState a -> a -> (a, GAState a)
    crossover :: GAState a -> (a, a) -> (a, a, GAState a)
    compute :: a -> [(String, Double)] -> Double

iterateGA :: (GAble a) => GAState a -> GAState a
iterateGA st = st'
    where st' = undefined

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
        where (p1:p2:_, gen) = nRands (randGen st) 2 :: ([Int], StdGen)
              m1' = swap'' (m1, p1) (m2, p2)
              m2' = swap'' (m2, p2) (m1, p1)
              st' = st { randGen = gen }
              swap'' (m1, p1) (m2, p2) = replaceSubMat (p1 `mod` m1s) (subTreeMat (p2 `mod` m2s) m2) m1
                    where m1s = undefined
                          m2s = undefined
    compute = evalMatrix

-- Utility stuff
nRands :: (RandomGen g, Random a) => g -> Int -> ([a], g)
nRands g n = foldl' step ([], g) [1..n]
    where step (l, g') _ = (fst p : l, snd p)
            where p = random g'

randElem :: [a] -> Double -> a
randElem xs r = xs !! floor (r * fromIntegral (length xs))

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (x:xs) 0 x' = x' : xs
replaceElem (x:xs) n x' = x : replaceElem xs (n - 1) x'

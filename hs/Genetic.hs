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
        fitFunc :: a -> Double
    }

data GAState a = GAState {
        config :: GAConfig a,
        randGen :: StdGen,
        iter :: Int,
        population :: [a]
    }

class GAble a where
    mutate :: GAState a -> a -> (a, GAState a)
    crossover :: GAState a -> (a, a) -> (a, a)

instance GAble IncMatrix where
    mutate st m = (m', st)
        where (t1:t2:t3:xs, gen) = nRands (randGen st) 3
              opIdx = floor $ fromIntegral (rows (numMatrix m) + 1) * t1
              newOp = case ops m !! opIdx of
                    LeafCNode _ -> leafPermute (t2, t3) (config st)
                    LeafTNode _ -> leafPermute (t2, t3) (config st)
                    UnNode f -> unaryPermute t2 (config st)
                    BinNode f -> binaryPermute t2 (config st)
              m' = m { ops = replaceElem (ops m) opIdx newOp }

    crossover = undefined

leafPermute (t1, t2) c | t1 >= 0.5 = LeafCNode $ t2 * 100
                       | otherwise = LeafTNode $ randElem (vars c) t2
unaryPermute t1 c = UnNode $ randElem (unaryOpsPool c) t1
binaryPermute t1 c = BinNode $ randElem (binaryOpsPool c) t1

-- Utility stuff
nRands :: (RandomGen g, Random a) => g -> Int -> ([a], g)
nRands g n = foldl' step ([], g) [1..n]
    where step (l, g') _ = (fst pair : l, snd pair)
          pair = random g

randElem :: [a] -> Double -> a
randElem xs r = xs !! floor (r * fromIntegral (length xs))

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (x:xs) 0 x' = x' : xs
replaceElem (x:xs) n x' = x : replaceElem xs (n - 1) x'

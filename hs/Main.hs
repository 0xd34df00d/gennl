{-# LANGUAGE FlexibleInstances #-}

import System.Environment
import Control.Arrow
import Data.List
import Data.Either
import Random
import Debug.Trace
import Numeric.FAD

import ExprTree
import NaturalParser
--import ExprIncidenceMatrix
import Genetic
import CSVParser
import Formattable

instance (Read a, Num a) => Read (Dual tag a) where
    readsPrec i s = map (first Numeric.FAD.lift) (readsPrec i s)

applyP f s = case s of
        Right t -> Right $ f t
        Left e -> Left e
applyP' f s = applyP f $ parseStr s

simplified s = applyP' simplifyStab s
--incMatrix s = applyP' toIncMatrix s

runStuff (Left m) _ _ = error $ show m
runStuff (Right recs) num g = runStuff' (map (map read) recs) num g

runStuff' recs num g = (pretty a, fit, iter st, map snd $ fits st)
    where defGA = initPpl num $ initGA (cfg { vars = ["x", "y"], testSet = map (take 2 &&& head . drop 2) recs, optNum = num } ) g
          cfg = defConfig :: GAConfig (ExprTree (Double))
          (a, fit, st) = runGA defGA

genSynth f ni nj = intercalate "\r\n" $ map (intercalate "," . map show) [ [i, j, f i j ] | i <- [1..ni], j <- [1..nj] ]

main' :: Int -> Double -> IO ()
main' num pts = do
    let recs = parseCSV (genSynth (\x y -> (exp 1) ** ((x / y + y / x) / 10) + 1) pts pts ++ "\r\n")
    --file <- readFile "options.dat.txt"
    --let recs = parseCSV file
    g <- newStdGen
    print $ runStuff recs num g

main = do
    args <- getArgs
    main' (read $ args !! 0) (read $ args !! 1)

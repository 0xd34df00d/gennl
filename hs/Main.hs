{-# LANGUAGE FlexibleInstances #-}

import System.Environment
import Control.Arrow
import Data.List
import Data.Either
import Data.Functor ((<$>))
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

parsed s = applyP' id s
simplified s = applyP' simplifyStab s
--incMatrix s = applyP' toIncMatrix s

runStuff (Left m) _ _ _ = error $ show m
runStuff (Right recs) num pts g = runStuff' (map (map read) (take (pts * pts) recs')) num g
    where recs' = map (\(x:y:z:[]) -> y:z:x:[]) recs

runStuff' recs num g = (pretty a, fit, iter st, map snd $ fits st)
    where defGA = initPpl num $ initGA (cfg { vars = ["x", "y"], testSet = map (take 2 &&& head . drop 2) recs, optNum = num } ) g
          cfg = defConfig :: GAConfig (ExprTree (Double))
          (a, fit, st) = runGA defGA

genSynth f ni nj = intercalate "\r\n" $ map (intercalate "," . map show) [ [i, j, f i j ] | i <- map (+3) [1..ni], j <- map (+3) [1..nj] ]

main' :: Int -> Double -> IO ()
main' num pts = do
    --let recs = parseCSV (genSynth (\x y -> (exp 1) ** ((x / y + y / x) / 10) + 1) pts pts ++ "\r\n")
    --let recs = parseCSV (genSynth (\x y -> x * cos (y)) pts pts ++ "\r\n")
    file <- readFile "options.dat.txt"
    let recs = parseCSV file
    g <- newStdGen
    print $ runStuff recs num (floor pts) g

sse = do
    file <- readFile "options.dat.txt"
    let recs = parseCSV file
    print $ sse' recs

sse' (Left _) = error ":("
sse' (Right recs) = sse'' $ (read <$>) <$> recs

sse'' recs = sum $ map (\(z:x:y:[]) -> (f x y - z) ** 2) recs
    where f x y = ((3.86e11)/((((1.227e11/(x**y))*y)-2.46e8)/(cos ((((-5.89e-3+y)/((y-5.47e-3)/(cos (y)*y)))/y))*y)))/(cos (y)*((y**y)+(y*x)))

main = do
    args <- getArgs
    main' (read $ args !! 0) (read $ args !! 1)

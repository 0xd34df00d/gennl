import System.Environment
import Control.Arrow
import Data.List
import Data.Either
import Debug.Trace

import ExprTree
import NaturalParser
import ExprIncidenceMatrix
import Genetic
import CSVParser
import Random


applyP f s = case s of
        Right t -> Right $ f t
        Left e -> Left e
applyP' f s = applyP f $ parseStr s

simplified s = applyP' simplifyStab s
incMatrix s = applyP' toIncMatrix s

num = 1000

runStuff _ (Left m) = error $ show m
runStuff g (Right recs) = runStuff' g (map (map read) recs)

runStuff' g recs = (a, fit, iter st, map snd $ fits st)
    where defGA = initPpl num $ initGA (cfg { vars = ["x", "y"], testSet = map (take 2 &&& head . drop 2) recs } ) g
          cfg = defConfig :: GAConfig IncMatrix
          (a, fit, st) = runGA defGA

genSynth f ni nj = intercalate "\r\n" $ map (intercalate "," . map show) [ [i, j, f i j ] | i <- [1..ni], j <- [1..nj] ]

main = do
    --file <- readFile "options.dat.txt"
    let recs = parseCSV $ (genSynth (*) 5 5 ++ "\r\n")
    g <- newStdGen
    print $ runStuff g recs

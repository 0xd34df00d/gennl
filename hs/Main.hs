import System.Environment
import Control.Arrow
import Data.List
import Data.Either
import Random
import Debug.Trace

import ExprTree
import NaturalParser
import ExprIncidenceMatrix
import Genetic
import CSVParser
import Formattable

applyP f s = case s of
        Right t -> Right $ f t
        Left e -> Left e
applyP' f s = applyP f $ parseStr s

simplified s = applyP' simplifyStab s
incMatrix s = applyP' toIncMatrix s

runStuff (Left m) _ _ = error $ show m
runStuff (Right recs) num g = runStuff' (map (map read) recs) num g

runStuff' recs num g = (pretty a, fit, iter st, map snd $ fits st)
    where defGA = initPpl num $ initGA (cfg { vars = ["x", "y"], testSet = map (take 2 &&& head . drop 2) recs, optNum = num } ) g
          cfg = defConfig :: GAConfig ExprTree
          (a, fit, st) = runGA defGA

genSynth f ni nj = intercalate "\r\n" $ map (intercalate "," . map show) [ [i, j, f i j ] | i <- [0.1,0.2..ni/10], j <- [0.1,0.2..nj/10] ]

main' num pts = do
    let recs = parseCSV (genSynth (\x y -> x * y + x * x * y * y) pts pts ++ "\r\n")
    g <- newStdGen
    print $ runStuff recs num g

main = do
    args <- getArgs
    main' (read $ args !! 0) (read $ args !! 1)
    --file <- readFile "options.dat.txt"

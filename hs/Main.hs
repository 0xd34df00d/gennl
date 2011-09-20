import System.Environment

import ExprTree
import NaturalParser
import ExprIncidenceMatrix
import Genetic
import CSVParser
import Random

import Data.List

applyP f s = case s of
        Right t -> Right $ f t
        Left e -> Left e
applyP' f s = applyP f $ parseStr s

simplified s = applyP' simplifyStab s
incMatrix s = applyP' toIncMatrix s

num = 2

runStuff g = ppl defGA
    where defGA = initPpl num $ initGA (cfg { vars = ["x", "y"] } ) g
          cfg = defConfig :: GAConfig IncMatrix

main = do
    g <- newStdGen
    let trees = take num $ unfoldr (Just . randExprTree ["x", "y"] 100) g
    mapM_ print trees
    mapM_ (print . toIncMatrix) trees

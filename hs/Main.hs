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

runStuff g = show $ ppl defGA
    where defGA = initPpl 10 $ initGA (cfg { vars = ["x", "y"] } ) g
          cfg = defConfig :: GAConfig IncMatrix

main = do
    g <- newStdGen
    --return $ take 10 $ unfoldr (Just . randExprTree (["x", "y"])) g
    return $ runStuff g

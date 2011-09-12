import System.Environment

import ExprTree
import NaturalParser
import ExprIncidenceMatrix
import Genetic
import CSVParser

applyP f s = case s of
        Right t -> Right $ f t
        Left e -> Left e
applyP' f s = applyP f $ parseStr s

simplified s = applyP' simplifyStab s
incMatrix s = applyP' toIncMatrix s

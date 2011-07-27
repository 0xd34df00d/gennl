import System.Environment

import ExprTree
import NaturalParser
import ExprIncidenceMatrix
import Genetic

applyP f s = case parseStr s of
        Right t -> Right $ f t
        Left e -> Left e

simplified s = applyP simplifyStab s
incMatrix s = applyP toIncMatrix s

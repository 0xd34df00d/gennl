import System.Environment

import ExprTree
import NaturalParser
import ExprIncidenceMatrix

simplified str = case parseStr str of
        Right tree -> Right $ simplifyStab tree
        Left err -> Left err



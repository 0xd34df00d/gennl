module ExprIncidenceMatrix
    where

import Data.Packed.Matrix
import ExprTree

data NodeType = BinNode BinaryFunc
                | UnNode UnaryFunc
                | LeafCNode Const
                | LeafTNode String
    deriving (Show)

data IncMatrix = IncMatrix {
        numMatrix :: Matrix Double,
        ops :: [NodeType]
    }
    deriving (Show)

data VecState = VecState {
        pos :: Int,
        nodesList :: [NodeType],
        prevNodes :: [Int]
    }
    deriving (Show)

emptyVecState = VecState 0 [] []

(|++|) st n = st { nodesList = n : nodesList st, pos = pos st + 1 }
(|++-|) st b = st { prevNodes = b : prevNodes st }

vecTree t = st { nodesList = reverse $ nodesList st, prevNodes = reverse $ prevNodes st }
    where st = vecTree' emptyVecState t

vecTree' st (LeafVar (Var v)) = st |++| LeafTNode v
vecTree' st (LeafConst c) = st |++| LeafCNode c
vecTree' st (NodeUnary f r) = vecTree' (st |++| UnNode f |++-| pos st) r
vecTree' st (NodeBinary f l r) = vecTree' (st' |++-| b) r
    where st' = vecTree' (st |++| BinNode f |++-| b) l
          b = pos st

toIncMatrix t = IncMatrix (buildMatrix b (b - 1) f) (nodesList st)
    where st = vecTree t
          b = pos st
          f (i, j) | i == j + 1 = 1
                   | prevNodes st !! j == i = 1
                   | otherwise = 0

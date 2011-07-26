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
        opVec :: [NodeType]
    }
    deriving (Show)

toIncMatrix :: ExprTree -> IncMatrix
toIncMatrix = undefined

data VecState = VecState {
        pos :: Integer,
        nodesList :: [NodeType],
        prevNodes :: [Integer]
    }
    deriving (Show)

emptyVecState = VecState 0 [] []

(|++|) st n = st { nodesList = n : nodesList st, pos = pos st + 1 }
(|++-|) st b = st { prevNodes = b : prevNodes st }

vecTree t = st { nodesList = reverse $ nodesList st, prevNodes = reverse $ prevNodes st }
    where st = vecTree' emptyVecState t

vecTree' st (LeafVar (Var v)) = st |++| LeafTNode v
vecTree' st (LeafConst c) = st |++| LeafCNode c
vecTree' st (NodeUnary f r) = vecTree' (st |++| UnNode f |++-| (pos st)) r
vecTree' st (NodeBinary f l r) = vecTree' (st' |++-| b) r
    where st' = vecTree' (st |++| BinNode f |++-| b) l
          b = pos st

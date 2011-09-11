{-# LANGUAGE NoMonomorphismRestriction #-}

module ExprIncidenceMatrix
    where

import Data.Packed.Matrix
import Data.Packed.Vector
import ExprTree
import Debug.Trace

data NodeType = BinNode BinaryFunc
                | UnNode UnaryFunc
                | LeafCNode Const
                | LeafTNode String
    deriving (Show)

type NumMatrix = Matrix Double

data IncMatrix = IncMatrix {
        numMat :: NumMatrix,
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

evalMatrix :: IncMatrix -> [(String, Double)] -> Double
evalMatrix (IncMatrix numMat ops) list = undefined

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

replaceSubMat :: Int -> IncMatrix -> IncMatrix -> IncMatrix
replaceSubMat pos sub m = insertSubMat pos sub $ removeSubMat (pos, subTreeEndIdx (numMat m) pos) m

insertSubMat :: Int -> IncMatrix -> IncMatrix -> IncMatrix
insertSubMat pos sub m = IncMatrix (fromBlocks [[subMatrix (0, 0) (pos, pos) mnum] ++ blk1mid ++ blk1end]) ops'
    where ops' = take pos (ops m) ++ ops sub ++ drop pos (ops m)
          mnum = numMat m
          snum = numMat sub
          blk1mid | c > 1 = [zMat c]
                  | otherwise = []
            where c = cols snum
          blk1end | diff == 0 = []
                  | otherwise = [zMat diff]
            where diff = pos - cols mnum - 1

removeSubMat :: (Int, Int) -> IncMatrix -> IncMatrix
removeSubMat (pos, end) m = IncMatrix num' (except (pos, end) (ops m))
    where num' = (fromColumns . except (pos, end - 1) . toColumns . fromRows . except (pos, end) . toRows) (numMat m)

except (a, b) xs = take a xs ++ drop (b + 1) xs

zMat c = (c><c) (cycle [0])

subTreeMat :: Int -> IncMatrix -> IncMatrix
subTreeMat pos m | pos /= 0 = IncMatrix (subMatrix (pos, pos - 1) (size, size) (numMat m)) (take size $ drop pos $ ops m)
                 | otherwise = m
    where size = end - pos + 1
          end = subTreeEndIdx (numMat m) pos

subTreeEndIdx :: NumMatrix -> Int -> Int
subTreeEndIdx m pos = if last == pos - 1
                        then pos
                        else subTreeEndIdx m (findLastNZCol m last)
    where last = findLastNZRow m pos

findLastNZRow = findLastNZ toRows

findLastNZCol = findLastNZ toColumns

findLastNZ trans m row = foldVectorWithIndex folder 0 (trans m !! row)
    where folder idx elem last | elem /= 0 = max idx last
                               | otherwise = last

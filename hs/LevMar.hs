module LevMar
    where

import Control.Arrow
import Control.Monad
import Numeric.FAD

import Matrix
import SupportUtils

-- delta = (JtJ + lambda diag (JtJ))^-1 Jt (y - f(beta))

type Model a = ([a], [a]) -> a
type Jacob a = ([a], [a]) -> [a]

type MModel a = (Matrix a, Matrix a) -> a
type MJacob a = (Matrix a, Matrix a) -> Matrix a

vecFun :: Num a => MModel a -> Matrix a -> Matrix a -> Matrix a
vecFun f β xs = fromRows $ listize (map (\x -> f (β, x)) xs')
    where xs' = rowsAsMats xs

jacMat :: Num a => MJacob a -> Matrix a -> Matrix a -> Matrix a
jacMat j β xs = fromRows $ map (\x -> concat $ rows $ j (β, x)) xs'
    where xs' = rowsAsMats xs

f2v :: Model a -> MModel a
f2v f p = f (join (***) (concat . cols) p)

j2v :: Jacob a -> MJacob a
j2v j p = fromRows [j (join (***) (concat . cols) p)]

fitModel :: (Num a, Fractional a) => Model a -> Jacob a -> [(a, [a])] -> [a] -> [a]
fitModel f j pts β = concat $ cols $ fitModel' 0 (f2v f) (j2v j) yv xv βv
    where yv = fromRows $ map (\x -> [fst x]) pts
          xv = fromRows $ map snd pts
          βv = fromRows $ listize β

fitModel' :: (Num a, Fractional a) => Int -> MModel a -> MJacob a -> Matrix a -> Matrix a -> Matrix a -> Matrix a
fitModel' iter f j ys xs β | iter > 100 = β +|+ δ
                           | otherwise = fitModel' (iter + 1) f j ys xs (β +|+ δ)
    where δ = invMat (js +|+ λ *| diag js) *|* jmt *|* (ys -|- vecFun f β xs)
          jm = jacMat j β xs
          jmt = trp jm
          js = jmt *|* jm
          λ = 1

f1 :: Num a => Model a
f1 (a:b:[], x:y:[]) = a*x + b*y*y

j1 :: (Real a, Fractional a, Num a) => Jacob a
j1 (cs@(a:b:[]), xs@(x:y:[])) = concat $ jacobian (\cts -> [f1 (map realToFrac cs, cts)]) xs

xs = [[0.0, 0.0], [1.0, 1.0], [2.0, 1.0]]
ys = map (\x -> f1 ([4, 5], x)) xs
pts = zipWith (,) ys xs

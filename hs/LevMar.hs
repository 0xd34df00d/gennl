module LevMar
    where

import Control.Arrow
import Control.Monad
import Numeric.FAD
import Debug.Trace

import Matrix
import SupportUtils

-- delta = (JtJ + lambda diag (JtJ))^-1 Jt (y - f(beta))

type Model a = ([a], [a]) -> a
type Jacob a = ([a], [a]) -> [a]

type MModel a = (Matrix a, Matrix a) -> a
type MJacob a = (Matrix a, Matrix a) -> Matrix a

vecFun :: Num a => MModel a -> Matrix a -> Matrix a -> Matrix a
vecFun f β xs = fromCols [map (\x -> f (β, x)) xs']
    where xs' = rowsAsMats xs

jacMat :: Num a => MJacob a -> Matrix a -> Matrix a -> Matrix a
jacMat j β xs = fromRows $ map (\x -> concat $ rows $ j (β, x)) xs'
    where xs' = rowsAsMats xs

f2v :: Model a -> MModel a
f2v f p = f (join (***) (concat . cols) p)

j2v :: Jacob a -> MJacob a
j2v j p = fromRows [j (join (***) (concat . cols) p)]

modelSSE :: (Floating a) => MModel a -> (Matrix a, Matrix a) -> Matrix a -> a
modelSSE f (ys, xs) β = sqrt (absMVec (ys -|- vecFun f β xs))

fitModel :: (Ord a, RealFloat a) => Model a -> Jacob a -> [(a, [a])] -> [a] -> [a]
fitModel f j pts β = concat $ cols $ fitModel' 0 0.01 sse (f2v f) (j2v j) (yv, xv) βv
    where yv = fromCols [map fst pts]
          xv = fromRows $ map snd pts
          βv = fromCols [β]
          sse = modelSSE (f2v f) (yv, xv) βv

fitModel' :: (Ord a, RealFloat a) => Int -> a -> a -> MModel a -> MJacob a -> (Matrix a, Matrix a) -> Matrix a -> Matrix a
fitModel' iter λ sse f j (ys, xs) β | iter > 20 || shStop = β
                                    | otherwise = {-(sse, ssed, iter', λ', β') `traceShow`-} fitModel' iter' λ' sse' f j (ys, xs) β'
    where δfor λ = invMatLU (js +|+ λ *| diag js) *|* jmt *|* (ys -|- vecFun f β xs)
          jm = jacMat j β xs
          jmt = trp jm
          js = jmt *|* jm
          shStop | iter' > iter = absMVec (β' -|- β) < min 0.00001 (absMVec β / 50) || isInfinite sse || isInfinite ssed
                 | otherwise = λ > 1e9 || isInfinite sse || isInfinite ssed
          ν = 5.5
          (λd, λu) = (λ / ν, λ * ν)
          δd = δfor λd
          ssed = modelSSE f (ys, xs) (β +|+ δd)
          (iter', sse', λ', β') | ssed <= sse = (iter + 1, ssed, λd, β +|+ δd)
                                | otherwise = (iter, sse, λu, β)
--{-# SPECIALIZE fitModel' :: Int -> Double -> Double -> MModel Double -> MJacob Double -> (Matrix Double, Matrix Double) -> Matrix Double -> Matrix Double #-}

f1 :: Floating a => Model a
f1 (a:b:c:[], x:y:[]) = a * x + b * y + c * y * sin x

j1 :: (Real a, Fractional a, Floating a) => Jacob a
j1 (cs@(a:b:c:[]), xs@(x:y:[])) = concat $ jacobian (\cts -> [f1 (cts, map realToFrac xs)]) cs

xs = [[x, y] | x <- [0.0 .. 5.0], y <- [0.0 .. 5.0]]
ys = map (\x -> f1 ([4, 5, 2], x)) xs
pts = zip ys xs

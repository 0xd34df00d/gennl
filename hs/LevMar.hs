module LevMar
    where

import Control.Arrow
import Control.Monad
import Numeric.FAD

import Matrix
import SupportUtils

-- delta = (JtJ + lambda diag (JtJ))^-1 Jt (y - f(beta))

type Model a = (([a], [a]) -> a)
type Jacob a = (([a], [a]) -> [a])

type MModel a = ((Matrix a, Matrix a) -> a)
type MJacob a = ((Matrix a, Matrix a) -> Matrix a)

vecFun :: Num a => MModel a -> Matrix a -> Matrix a -> Matrix a
vecFun f β xs = fromLists $ listize (map (\x -> f (β, x)) xs')
    where xs' = rowsAsMats xs

jacMat :: Num a => MJacob a -> Matrix a -> Matrix a -> Matrix a
jacMat j β xs = fromLists $ map (\x -> concat $ rows $ j (β, x)) xs'
    where xs' = rowsAsMats xs

f2v :: Model a -> MModel a
f2v f p = f (join (***) (concat . cols) p)

j2v :: Jacob a -> MJacob a
j2v j p = fromLists [j (join (***) (concat . cols) p)]

fitModel :: Model Double -> Jacob Double -> [(Double, [Double])] -> [Double] -> [Double]
fitModel f j pts β = concat $ cols $ fitModel' 0 (f2v f) (j2v j) yv xv βv
    where yv = fromLists $ map (\x -> [fst x]) pts
          xv = fromLists $ map snd pts
          βv = fromLists $ listize β

fitModel' :: Int -> MModel Double -> MJacob Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
fitModel' iter f j ys xs β | iter > 10 = β +|+ δ
                           | otherwise = fitModel' (iter + 1) f j ys xs (β +|+ δ)
    where δ = (invMat (js +|+ λ *| diag js)) *|* jmt *|* (ys -|- vecFun f β xs)
          jm = jacMat j β xs
          jmt = trp jm
          js = jmt *|* jm
          λ = 0.1

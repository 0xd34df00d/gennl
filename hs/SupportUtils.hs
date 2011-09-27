{-# LANGUAGE DeriveDataTypeable #-}

module SupportUtils
    (
        TextableException,
        failStr,
        nRands,
        rndGens
    )
    where

import Control.Exception
import Control.Arrow
import Data.Typeable
import Data.List
import Numeric.FAD
import Random

data TextableException = TextableException String
    deriving (Typeable)

instance Show TextableException where
    show (TextableException str) = "Exception: " ++ str

instance Exception TextableException

instance (Random r, Num r, Real r, Fractional r) => Random (Dual b r) where
    random g = (first lift) (random g)
    randomR (l, r) g = (first lift) (randomR (realToFrac l, realToFrac r) g)

failStr str = throw $ TextableException str

nRands :: (RandomGen g, Random a) => g -> Int -> ([a], g)
nRands g n = (take n $ unfoldr (Just . random) g1, g2)
    where (g1, g2) = split g

rndGens :: (RandomGen g) => g -> [g]
rndGens = unfoldr (Just . split)

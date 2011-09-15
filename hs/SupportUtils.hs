{-# LANGUAGE DeriveDataTypeable #-}

module SupportUtils
    (
        TextableException,
        failStr,
        nRands
    )
    where

import Control.Exception
import Data.Typeable
import Data.List
import Random

data TextableException = TextableException String
    deriving (Typeable)

instance Show TextableException where
    show (TextableException str) = "Exception: " ++ str

instance Exception TextableException

failStr str = throw $ TextableException str

nRands :: (RandomGen g, Random a) => g -> Int -> ([a], g)
nRands g n = (take n $ unfoldr (Just . random) g1, g2)
    where (g1, g2) = split g

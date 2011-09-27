{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Formattable
    where

import Numeric.FAD

class Show a => Formattable a where
    pretty :: a -> String
    pretty = show

instance Formattable Int
instance Formattable Double

instance (Formattable b, Real b, Fractional b) => Formattable (Dual a b) where
    pretty d = pretty (realToFrac d :: Double)

instance (Formattable a, Formattable b) => Formattable (a, b) where
    pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

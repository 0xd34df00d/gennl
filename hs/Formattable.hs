module Formattable
    where

import Text.Printf

class Show a => Formattable a where
    pretty :: a -> String
    pretty = show

instance Formattable Int
instance Formattable Double where
    pretty = printf "%.4f"

instance (Formattable a, Formattable b) => Formattable (a, b) where
    pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

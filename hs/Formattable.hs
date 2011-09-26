module Formattable
    where

class Show a => Formattable a where
    pretty :: a -> String
    pretty = show

instance Formattable Int
instance Formattable Double

instance (Formattable a, Formattable b) => Formattable (a, b) where
    pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

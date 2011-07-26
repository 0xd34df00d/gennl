{-# LANGUAGE DeriveDataTypeable #-}

module SupportUtils
    (
        TextableException,
        failStr
    )
    where

import Control.Exception
import Data.Typeable

data TextableException = TextableException String
    deriving (Typeable)

instance Show TextableException where
    show (TextableException str) = "Exception: " ++ str

instance Exception TextableException

failStr str = throw $ TextableException str

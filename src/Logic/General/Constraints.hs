{-# LANGUAGE DeriveDataTypeable #-}
module Logic.General.Constraints where

import Logic.General.Entities
import Data.Typeable.Internal

data F = F { fName :: String }
       deriving (Eq,Show)

data Ec = Entity E
        | Apply F [Ec]
        | Value Value
        deriving (Eq,Show,Typeable)

data C = Boolean Bool
       | Equals Ec Ec
       | LessThan Ec Ec
       | GreaterThan Ec Ec
       | Not C
       | Conj C C
       deriving (Eq,Show)

data Value 
    = Int' Int
    | Float' Float
    | String' String
    | Bool' Bool
  deriving (Eq,Show)

instance Ord Ec where
  compare (Value (Int' a))   (Value (Int' b))   = compare a b
  compare (Value (Float' a)) (Value (Float' b)) = compare a b
  compare a b = error$"Comparing non-comparable things "++show a++" and "++show b

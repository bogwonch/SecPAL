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
       | Not C
       | Conj C C
       deriving (Eq,Show)

data Value 
    = Int' Int
    | Float' Float
    | String' String
    | Bool' Bool
  deriving (Eq,Show)


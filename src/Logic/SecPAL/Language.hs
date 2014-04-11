{-# LANGUAGE DeriveDataTypeable #-}
module Logic.SecPAL.Language where

import Data.Typeable.Internal

data E = Variable { varName :: String }
       | Constant { constName :: String }
       deriving (Show)

instance Eq E where
    (Variable _) == (Constant _) = False
    (Constant _) == (Variable _) = False
    (Variable x) == (Variable y) = x == y
    (Constant x) == (Constant y) = x == y

data D = Zero
       | Infinity
       deriving (Eq,Show)

data VerbPhrase = Predicate { predicate :: String, args :: [E] }
                | CanSay { delegation :: D, what :: Fact }
                | CanActAs { whom :: E }
                deriving (Eq,Show)

data Fact = Fact { subject :: E, verb :: VerbPhrase }
          deriving (Eq,Show)

data Claim = Claim { fact :: Fact, conditions :: [Fact], constraint :: C }
           deriving (Eq,Show)

data Assertion = Assertion { who :: E, says :: Claim }
               deriving (Eq,Show)

data AC = AC [Assertion]
        deriving (Eq,Show)

acs :: AC -> [Assertion]
acs (AC as) = as

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


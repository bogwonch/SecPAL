module Logic.SecPAL.Language where

import Logic.General.Entities
import Logic.General.Constraints

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


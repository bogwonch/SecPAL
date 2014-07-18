module Logic.DatalogC.Language where

import Data.List

data Clause = Clause{ head       :: Predicate
                    , body       :: [Predicate]
                    , constraint :: Constraint
                    }

-- A fact is a clause with no body (i.e. it is trivially true)
fact :: Clause
fact = Clause{ Logic.DatalogC.Language.head=undefined
             , body=[]
             , constraint=Boolean True 
             }

isFact :: Clause -> Bool
isFact f = null . body $ f

rule :: Clause
rule = Clause{ Logic.DatalogC.Language.head=undefined
             , body=undefined
             , constraint=Boolean True 
             }

data Predicate = Predicate{ name :: String, args :: [Entity] }

data Entity = Variable String
            | Constant String
            deriving (Eq, Show)

-- TODO: work out what constraints really are
data Constraint = Boolean Bool

-- Pretty printing
instance Show Clause where
  show f
    | isFact f  = show (Logic.DatalogC.Language.head f) ++ "."
    | otherwise = 
      show (Logic.DatalogC.Language.head f) 
           ++ " :- " 
           ++ intercalate ", " (map show (body f)) 
           ++ "."

instance Show Predicate where
  show p =
    name p ++ arity ++ arguments
    where
      arity     = "/" ++ (show . length . args $ p)
      arguments = "(" ++ intercalate ", " (map show (args p)) ++ ")"

instance Show Constraint where
  show (Boolean True) = ""
  show x = " ["++show' x++"]"
    where show' (Boolean False) = "False"
          show' (Boolean True) = error "wtf?"



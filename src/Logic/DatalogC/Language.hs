module Logic.DatalogC.Language where

import Data.List
import Logic.General.Entities
import qualified Logic.General.Named as N
import Logic.General.Constraints

data Clause = Clause{ head       :: Predicate
                    , body       :: [Predicate]
                    , constraint :: C
                    }
  deriving (Show)

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

data Predicate = Predicate{ name :: String, args :: [E] }
  deriving (Show)

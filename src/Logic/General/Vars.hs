{-- Extracts the free variables in a part of SecPAL
 - REFACTOR: Secpal code in secpal directory
 -}
module Logic.General.Vars where

import Logic.General.Entities
import Logic.General.Constraints
import Logic.SecPAL.Language
import qualified Logic.DatalogC.Language as L

class Vars a where
    vars :: a -> [E]

-- A phrase of syntax is ground when it contains no variables.
ground :: Vars a => a -> Bool
ground = (==0) . length . vars

instance Vars E where
    vars e@Variable{} = [e]
    vars Constant{} = []

instance Vars VerbPhrase where
    vars Predicate {args=a} = concatMap vars a
    vars CanSay {what=w} = vars w
    vars CanActAs {} = []

instance Vars Fact where
    vars Fact {subject=s, verb=v} = vars s ++ vars v

instance Vars Claim where
    vars Claim {fact=f, conditions=fs, constraint=c} = 
      vars f ++ concatMap vars fs ++ vars c

instance Vars Assertion where
    vars Assertion { who=w, says=s } = vars w ++ vars s

instance Vars AC where
    vars (AC ac) = concatMap vars ac

instance Vars Ec where
    vars (Entity e) = vars e
    vars (Apply _ es) = concatMap vars es
    vars (Value _) = []
    vars Fail{} = []

instance Vars C where
    vars Boolean {} = []
    vars (Equals e1 e2) = vars e1 ++ vars e2
    vars (LessThan e1 e2) = vars e1 ++ vars e2
    vars (GreaterThan e1 e2) = vars e1 ++ vars e2
    vars (Not c) = vars c
    vars (Conj c1 c2) = vars c1 ++ vars c2

instance Vars L.Predicate where
  vars L.Predicate{ L.args=as } = concatMap vars as

instance Vars L.Clause where 
  vars c = vars (L.head c) ++ vars (L.body c) ++ vars (L.constraint c)

instance Vars x => Vars [x] where
  vars = concatMap vars

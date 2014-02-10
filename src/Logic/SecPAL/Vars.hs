module Logic.SecPAL.Vars where

import Logic.SecPAL.Language

class Vars a where
    vars :: a -> [E]

-- A phrase of syntax is ground when it contains no variables.
ground :: Vars a => a -> Bool
ground = (==0) . length . vars

instance Vars E where
    vars e@(Variable _) = [e]
    vars (Constant _) = []

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

instance Vars C where
    vars Boolean {} = []
    vars (Equals e1 e2) = vars e1 ++ vars e2
    vars (Not c) = vars c
    vars (Conj c1 c2) = vars c1 ++ vars c2

module Logic.SecPAL.Substitutions where

import Logic.SecPAL.Language

data Substitution = Substitute{ var::E, for::E }
  deriving (Eq)

instance Show Substitution where
  show Substitute{var=v, for=f} = "("++show v++"\\"++show f++")"

class Substitutive a where
  sub :: a -> Substitution -> a 

instance Substitutive E where
  x `sub` θ  
    | x == var θ = var θ
    | otherwise  = x

-- There is almost certainly a better way using applicative
instance Substitutive VerbPhrase where
  x@Predicate{args=a} `sub` θ = x{args=map (`sub` θ) a}
  x@CanSay{what=w}    `sub` θ = x{what=w `sub` θ}
  x@CanActAs{whom=w}  `sub` θ = x{whom=w `sub` θ}

instance Substitutive Fact where
  f@Fact{ subject=s, verb=v } `sub` θ =
    f{ subject=s `sub` θ
     , verb=v `sub` θ
     }

instance Substitutive Claim where
  x@Claim{fact=f, conditions=fs, constraint=c} `sub` θ =
    x{ fact       = f `sub` θ
     , conditions = map (`sub` θ) fs
     , constraint = c `sub` θ
     }

instance Substitutive Assertion where
  a@Assertion{who=w, says=s} `sub` θ = 
    a{ who=w `sub` θ
     , says=s `sub` θ
     }

instance Substitutive AC where
  (AC as) `sub` θ = AC $ map (`sub` θ) as

instance Substitutive Ec where
  (Entity e)   `sub` θ = Entity $ e `sub` θ
  (Apply f es) `sub` θ = Apply f (map (`sub` θ) es)

instance Substitutive C where
  (Boolean b) `sub` θ  = Boolean b
  (Equals a b) `sub` θ = Equals (a `sub` θ) (b `sub` θ)
  (Not a) `sub` θ      = Not $ a `sub` θ
  (Conj a b) `sub` θ   = Conj (a `sub` θ) (b `sub` θ)


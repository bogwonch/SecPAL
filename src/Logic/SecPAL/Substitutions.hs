module Logic.SecPAL.Substitutions where

import Logic.SecPAL.Language
import Data.Maybe

data Substitution = Substitute{ var::E, for::E }
  deriving (Eq)

rename :: E -> E -> Substitution
a `rename` b = Substitute{ var=a, for=b }

instance Show Substitution where
  show Substitute{var=v, for=f} = "("++show v++"\\"++show f++")"

class Substitutive a where
  sub :: a -> Substitution -> a 
  (==?) :: a -> a -> Maybe [Substitution]

instance Substitutive E where
  x `sub` θ  
    | x == var θ = var θ
    | otherwise  = x

  a@(Constant{}) ==? b@(Constant{})
    | a == b = Just []
    | otherwise = Nothing

  a@(Constant{}) ==? b@(Variable{}) = Just [b `rename` a]
  a@(Variable{}) ==? b@(Constant{}) = Just [a `rename` b]

  _ ==? _ = Nothing

-- There is almost certainly a better way using applicative
instance Substitutive VerbPhrase where
  x@Predicate{args=a} `sub` θ = x{args=map (`sub` θ) a}
  x@CanSay{what=w}    `sub` θ = x{what=w `sub` θ}
  x@CanActAs{whom=w}  `sub` θ = x{whom=w `sub` θ}

  p@Predicate{} ==? q@Predicate{}
    | predicate p == predicate q = 
        let es = zipWith (==?) (args p) (args q)
        in if all isJust es
             then Just . concat . catMaybes $ es
             else Nothing
    | otherwise = Nothing

  a@CanSay{} ==? b@CanSay{}
    | delegation a == delegation b = what a ==? what b
    | otherwise = Nothing

  _ ==? _ = Nothing

instance Substitutive Fact where
  f@Fact{ subject=s, verb=v } `sub` θ =
    f{ subject=s `sub` θ
     , verb=v `sub` θ
     }

  f@Fact{} ==? f'@Fact{} =
    let ss = subject f ==? subject f'
        vs = verb f ==? verb f'
    in if isJust ss && isJust vs
         then Just $ fromJust ss ++ fromJust vs
         else Nothing

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
  v@(Value _)  `sub` _ = v

  (Value a) ==? (Value b)
    | a == b = Just []
    | otherwise = Nothing

  (Entity a) ==? (Entity b) = a ==? b

  (Apply f es) ==? (Apply f' es')
    | f /= f' = Nothing
    | otherwise = let renamed_es = zipWith (==?) es es'
                      renamable = all isJust renamed_es
                  in if renamable 
                       then Just . concat . catMaybes $ renamed_es
                       else Nothing

  _ ==? _ = Nothing

instance Substitutive C where
  (Boolean b) `sub` _  = Boolean b
  (Equals a b) `sub` θ = Equals (a `sub` θ) (b `sub` θ)
  (Not a) `sub` θ      = Not $ a `sub` θ
  (Conj a b) `sub` θ   = Conj (a `sub` θ) (b `sub` θ)

  (Conj a b) ==? (Conj a' b') =
    let as = a ==? a'
        bs = b ==? b'
    in if isJust as && isJust bs 
         then Just . concat . catMaybes $ [as,bs]
         else Nothing

  (Not a) ==? (Not b) = a ==? b

  (Equals a b) ==? (Equals a' b') =
    let as = a ==? a'
        bs = b ==? b'
    in if isJust as && isJust bs
         then Just . concat .catMaybes $ [as,bs]
         else Nothing

  (Boolean a) ==? (Boolean b)
    | a == b = Just []
    | otherwise = Nothing

  _ ==? _ = Nothing

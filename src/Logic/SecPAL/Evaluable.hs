module Logic.SecPAL.Evaluable where

import Logic.SecPAL.Language
import Logic.SecPAL.Context
import Logic.SecPAL.AssertionSafety (flat)

--import Debug.Trace

class Evaluable x where 
  (||-) :: Context -> x -> Bool

instance Evaluable C where
    ctx ||- c = case c of
                  (Boolean b) -> b
                  (Equals x y) -> x == y
                  (Not c') -> not (ctx ||- c)
                  (Conj x y) -> (ctx ||- x) && (ctx ||- y)

instance Evaluable Assertion where
  ctx ||- x 
    -- If x is in the assertion context then we're done
    -- | x `isIn` ac ctx = trace (show x++" isIn "++show ctx) True
    | x `isIn` ac ctx = True
    | otherwise = tryCond ctx x


isIn :: Assertion -> AC -> Bool
x `isIn` (AC xs) = x `elem` xs

-- The Glorious Cond Rule!
--
-- (A says f if f_1, .., f_n, c) ∈ AC
-- AC,D |= A says f_i, ∀i ∈ (1..n)    |= c    flat(f)
-- --------------------------------------------------
--                  AC, D |= A says f
--
cond :: Context -> Assertion -> Assertion -> Bool
cond ctx result query =
    let whom = who query
        whoSays = asserts whom
        fs = conditions (says query)
        aSaysFs = map whoSays fs
    in
      all (ctx ||-) aSaysFs &&
      ctx ||- (constraint . says $ query) &&
      (flat . fact . says $ query)

asserts :: E -> Fact -> Assertion
a `asserts` f = Assertion { who=a
                          , says = Claim { fact=f
                                         , conditions=[]
                                         , constraint=Boolean True
                                         }
                          } 

tryCond :: Context -> Assertion -> Bool
tryCond ctx a = 
  let as = filter (isSpecific a) (acs (ac ctx))
  in any (cond ctx a) as

isSpecific :: Assertion -> Assertion -> Bool
x `isSpecific` y = 
    let 
      who_x = who x
      who_y = who y
      says_x = says x
      says_y = says y
      fact_x = fact says_x
      fact_y = fact says_y
      result = ((who_x == who_y) && (fact_x == fact_y))
    in 
      --trace (show x ++ (if result then " <=== " else " <=/= ") ++ show y) $ 
      result







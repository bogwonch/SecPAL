module Logic.SecPAL.Evaluable where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
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
      | x `isIn` ac ctx = True
      | otherwise = tryCond ctx x || 
                    tryCanSay ctx x


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

-- The Mysterious Can-Say Rule!
--
-- AC, oo |= A says B can-say D fact    AC, D |= B says fact
-- ---------------------------------------------------------
--                     AC, oo |= A says fact
canSay :: Context -> Assertion -> Assertion -> Bool
canSay ctx@Context{d=Zero} _ _ = False
canSay ctx query canSayStm = 
  let whom = who query
      delegate = who canSayStm
      f = fact . says $ query
      cs = fact . says $ canSayStm
      f' = what . verb $ cs
      d = delegation . verb $ cs
      b = subject cs
  in
    f == f' &&
    ctx ||- delegates whom b f d &&
    ctx{d=d} ||- (b `asserts` f)


asserts :: E -> Fact -> Assertion
a `asserts` f = Assertion { who=a
                          , says = Claim { fact=f
                                         , conditions=[]
                                         , constraint=Boolean True
                                         }
                          }

delegates :: E -> E -> Fact -> D -> Assertion
delegates from to what level =
    Assertion { who=from
              , says = Claim { fact = Fact { subject = to
                                           , verb = CanSay level what
                                           }
                             , conditions=[]
                             , constraint=Boolean True
                             }
              }

tryCond :: Context -> Assertion -> Bool
tryCond ctx a = 
    let as = filter (isSpecific a) (acs (ac ctx))
    in any (cond ctx a) as


tryCanSay :: Context -> Assertion -> Bool
tryCanSay ctx a = 
    let as = filter (isDelegation a) (acs (ac ctx))
    in any (canSay ctx a) as

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


isDelegation :: Assertion -> Assertion -> Bool
isDelegation 
  x@Assertion{ who=a
             , says=Claim{fact=f}
             } 
  y@Assertion{ who=a'
             , says=Claim{ fact=Fact{ subject=b
                                    , verb=CanSay{what=f'}
                                    }
                         }
             }
    = (a == a') && (f == f')
isDelegation _ _ = False






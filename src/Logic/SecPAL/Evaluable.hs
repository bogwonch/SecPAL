module Logic.SecPAL.Evaluable where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.SecPAL.Context
import Logic.SecPAL.AssertionSafety (flat)
import Logic.SecPAL.Proof hiding (constraint, delegation)
import Data.Maybe

--import Debug.Trace

class Evaluable x where 
    (||-) :: Context -> x -> Maybe (Proof x)

instance Evaluable C where
  {-
    ctx ||- c = case c of
                  (Boolean b) -> b
                  (Equals x y) -> x == y
                  (Not c') -> not (ctx ||- c)
                  (Conj x y) -> (ctx ||- x) && (ctx ||- y)
  -}
  
  ctx ||- c@(Boolean True) = Just $ PStated (ctx,c)
  ctx ||- c@(Boolean False) = Nothing

  ctx ||- c@(Equals a b)
    | a == b = Just $ PStated (ctx,c)
    | otherwise = Nothing

  ctx ||- c@(Not c') = 
    let p = isJust $ ctx ||- c'
    in if p 
         then Just $ PStated (ctx,c)
         else Nothing

  ctx ||- c@(Conj x y) =
    let pX = isJust $ ctx ||- x
        pY = isJust $ ctx ||- y
    in if pX && pY 
         then Just $ PStated (ctx,c)
         else Nothing


instance Evaluable Assertion where
    ctx ||- x 
    -- If x is in the assertion context then we're done
      | x `isIn` ac ctx = Just $ PStated (ctx,x)
      | otherwise = let tC = tryCond ctx x 
                        tCS = tryCanSay ctx x
                    in if isJust tC then tC else tCS


isIn :: Assertion -> AC -> Bool
x `isIn` (AC xs) = x `elem` xs

-- The Glorious Cond Rule!
--
-- (A says f if f_1, .., f_n, c) ∈ AC
-- AC,D |= A says f_i, ∀i ∈ (1..n)    |= c    flat(f)
-- --------------------------------------------------
--                  AC, D |= A says f
--
cond :: Context -> Assertion -> Assertion -> Maybe (Proof Assertion)
cond ctx result query =
    let whom = who query
        whoSays = asserts whom
        fs = conditions (says query)
        aSaysFs = map whoSays fs
        in 
          makeCond (ctx,result) 
                   (map (ctx ||-) aSaysFs) 
                   (ctx ||- (constraint . says $ query)) 
                   (flat . fact . says $ query)



-- The Mysterious Can-Say Rule!
--
-- AC, oo |= A says B can-say D fact    AC, D |= B says fact
-- ---------------------------------------------------------
--                     AC, oo |= A says fact
canSay :: Context -> Assertion -> Assertion -> Maybe (Proof Assertion)
canSay ctx@Context{d=Zero} _ _ = Nothing
canSay ctx query canSayStm = 
  let whom = who query
      delegate = who canSayStm
      f = fact . says $ query
      cs = fact . says $ canSayStm
      f' = what . verb $ cs
      d = delegation . verb $ cs
      b = subject cs
  in if f == f'
       then makeCanSay (ctx,query) 
                       (ctx ||- delegates whom b f d) 
                       (ctx{d=d} ||- (b `asserts` f))
       else Nothing


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

tryCond :: Context -> Assertion -> Maybe (Proof Assertion)
tryCond ctx a = 
    let as = filter (isSpecific a) (acs (ac ctx))
    in case filter isJust $ map (cond ctx a) as of
         [] -> Nothing
         (p:_) -> p


tryCanSay :: Context -> Assertion -> Maybe (Proof Assertion)
tryCanSay ctx a = 
    let as = filter (isDelegation a) (acs (ac ctx))
    in case filter isJust $ map (canSay ctx a) as of
         [] -> Nothing
         (p:_) -> p

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



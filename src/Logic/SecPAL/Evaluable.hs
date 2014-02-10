{-# OPTIONS -Wall #-}
module Logic.SecPAL.Evaluable where

import Logic.SecPAL.Language
import Logic.SecPAL.Context
import Logic.SecPAL.AssertionSafety (flat)
import Logic.SecPAL.Proof hiding (constraint, delegation)
import Logic.SecPAL.Substitutions
import Data.Maybe

import Debug.Trace
import Logic.SecPAL.Pretty

class Evaluable x where 
    (||-) :: Context -> x -> Maybe (Proof x)

instance Evaluable C where
  ctx ||- c@(Boolean True) = Just $ PStated (ctx,c)
  _ ||- (Boolean False) = Nothing

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
      | isJust tS = tS
      | isJust tC = tC
      | otherwise = tCS
      where  
        tS = tryStated ctx x
        tC = tryCond ctx x 
        tCS = tryCanSay ctx x


isIn :: Assertion -> AC -> Bool
x `isIn` (AC xs) = x `elem` xs

-- The Glorious Cond Rule!
--
-- (A says f if f_1, .., f_n, c) ∈ AC
-- AC,D |= A says f_i, ∀i ∈ (1..n)    |= c    flat(f)
-- --------------------------------------------------
--                  AC, D |= A says f
--
cond' :: Context -> Assertion -> Assertion -> Maybe (Proof Assertion)
cond' ctx result query =
    let w = who query
        whoSays = asserts w
        fs = conditions (says query)
        aSaysFs = map whoSays fs
        ctx' = ctx{theta=[]}
    in
      makeCond (ctx,result) 
               (map (ctx' ||-) aSaysFs) 
               (ctx' ||- (constraint . says $ query)) 
               (flat . fact . says $ query)


cond :: Context -> Assertion -> Assertion -> Maybe (Proof Assertion)
cond ctx result query =
  let query'        = simplify query
      renaming      = fromJust $ result ==? query'
      renamedQuery  = subAll query renaming
      renamedResult = subAll result renaming
  in 
      cond' ctx{theta=renaming} renamedResult renamedQuery
  where
    simplify q = 
      let says' = (says q){ conditions=[], constraint=Boolean True }
          q' = q{ says=says' }
      in q'

-- The Mysterious Can-Say Rule!
--
-- AC, oo |= A says B can-say D fact    AC, D |= B says fact
-- ---------------------------------------------------------
--                     AC, oo |= A says fact
canSay' :: Context -> Assertion -> Assertion -> Maybe (Proof Assertion)
canSay' Context{d=Zero} _ _ = Nothing
canSay' ctx query canSayStm = 
  let w = who query
      f = fact . says $ query
      cs = fact . says $ canSayStm
      f' = what . verb $ cs
      de = delegation . verb $ cs
      b = subject cs
      ctx' = ctx{theta=[]}
  in if f == f'
       then makeCanSay (ctx,query) 
                       (ctx' ||- delegates w b f de) 
                       (ctx'{d=de} ||- (b `asserts` f))
       else Nothing

canSay :: Context -> Assertion -> Assertion -> Maybe (Proof Assertion)
canSay ctx result canSayStm = 
  let canSayStm'    = simplify canSayStm
      pRenaming     = result ==? canSayStm'
      renaming      = fromMaybe (error "can say statement failed to simplify") pRenaming 
      renamedCSS    = subAll canSayStm renaming
      renamedResult = subAll result renaming
  in
    canSay' ctx{theta=renaming} renamedResult renamedCSS
  where
    simplify q = 
      let f = what . verb . fact . says $ q
          claim' = (says q){ fact = f, conditions=[], constraint=Boolean True }
      in q{ says=claim' }

asserts :: E -> Fact -> Assertion
a `asserts` f = Assertion { who=a
                          , says = Claim { fact=f
                                         , conditions=[]
                                         , constraint=Boolean True
                                         }
                          }

delegates :: E -> E -> Fact -> D -> Assertion
delegates from to w level =
    Assertion { who=from
              , says = Claim { fact = Fact { subject = to
                                           , verb = CanSay level w
                                           }
                             , conditions=[]
                             , constraint=Boolean True
                             }
              }

tryStated :: Context -> Assertion -> Maybe (Proof Assertion)
tryStated ctx a =  
  let as = filter (isJust . (==? a)) (acs $ ac ctx)
  in case as of
    [] -> Nothing
    (p:_) -> 
      let ts = fromJust $ p ==? a
      in Just $ PStated (ctx{theta=ts}, p)

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

        whose = isJust $ who_x ==? who_y
        facts = isJust $ fact_x ==? fact_y
        result = whose && facts
    in 
        --trace (show x ++ (if result then " <=== " else " <=/= ") ++ show y) $ 
        result


isDelegation :: Assertion -> Assertion -> Bool
isDelegation 
  Assertion{ who=a
           , says=Claim{fact=f}
           } 
  Assertion{ who=a'
           , says=Claim{ fact=Fact{ verb=CanSay{what=f'} } }
           }
    = isJust (a ==? a') && isJust (f ==? f')
isDelegation _ _ = False



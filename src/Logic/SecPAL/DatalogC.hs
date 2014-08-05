module Logic.SecPAL.DatalogC where

import Control.Applicative
import Data.List
import Logic.General.Entities
import Logic.General.Fresh
import Logic.SecPAL.AssertionSafety
import qualified Logic.SecPAL.Language as SP

data Says = Says E E SP.Fact
data Rule = Rule Says [Says] SP.C

instance Show Says where
  show (Says a (Constant "secpal:zero") f) = show a++" says_0 "++show f
  show (Says a (Constant "secpal:inf") f) = show a++" says_inf "++show f
  show (Says a _ f) = show a++" says_k "++show f

instance Show Rule where
  show (Rule h bs _) = show h++" <- "++intercalate ", " (map show bs)++"."

fact :: Int -> SP.Assertion -> SP.Fact
fact 0 = SP.fact . SP.says
fact n = (!! n) . SP.conditions . SP.says

factToName :: SP.Fact -> String
factToName = vpToName . SP.verb

factToArgs :: SP.Fact -> [E]
factToArgs SP.Fact{ SP.subject=a, SP.verb=v } = a : vpToArgs v

vpToName :: SP.VerbPhrase -> String
vpToName SP.Predicate{ SP.predicate=name } = name
vpToName SP.CanActAs{} = "can_act_as"
vpToName SP.CanSay{ SP.delegation=d, SP.what=f } =
  let depth = if d==SP.Zero then "zero" else "infinity"
  in "can_say_"++depth++factToName f

vpToArgs :: SP.VerbPhrase -> [E]
vpToArgs SP.Predicate{ SP.args=es } = es
vpToArgs SP.CanActAs{ SP.whom=e } = [e]
vpToArgs SP.CanSay{ SP.what=f } = factToArgs f

translate :: SP.Assertion -> FreshState [Rule]
translate a
  | flat . fact 0 $ a = step1 a
  | otherwise = undefined


{- Algorithm 5.2. 
 - The translation of an assertion context AC proceeds as follows:
 -
 - 1. If fact0 is flat, then an assertion A says fact0 if fact1,...,factn,c is
 - translated into the clause A saysk fact0 ← A saysk fact1,...,A saysk factn,c
 - where k is a fresh variable.
 -}
step1 :: SP.Assertion -> FreshState [Rule]
step1 x = do
  let a = SP.who x
  k <- getFresh
  let h = Says a k (fact 0 x)
  let b = map (Says a k) (SP.conditions . SP.says $ x)
  let c = SP.constraint . SP.says $ x
  
  return [Rule h b c]
  
{- 2. Otherwise, fact0 is of the form 
 -    e0 can sayK0 ... en−1 can sayKn−1 fact,
 - for some n ≥ 1, where fact is flat. 
 -
 - Let factn ≡ fact 
 - and facti ≡ ei can sayKi facti+1, for i ∈ {0..n − 1}.
 - 
 - Note that fact0 = fact0. Then the assertion A says fact0 if fact1, ...,
 - factm, c is translated into a set of n + 1 Datalog rules as follows. 
 -
 - (a) We add the Datalog rule 
 -   A saysk fact0 ← A saysk fact1,...,A saysk factm,c 
 - where k is a fresh variable.
 -
 - (b) For each i ∈ {1..n}, we add a Datalog rule
 -    A says∞ facti ← 
 -      x saysKi−1 facti,
 -      A says∞ x can sayKi−1 fact i 
 - where x is a fresh variable.
 -}
step2 :: SP.Assertion -> FreshState [Rule]
step2 _ = undefined

step2a :: SP.Assertion -> FreshState [Rule]
step2a = step1

step2b :: SP.Assertion -> FreshState [Rule]
step2b ass@SP.Assertion{ SP.who=a
                       , SP.says=SP.Claim{ SP.fact=f }
                       }
  = let k = ks ass
    in step2b' 0 [] a k f

step2b' :: Int -> [Rule] -> E -> [SP.D] -> SP.Fact -> FreshState [Rule]
step2b' 0 rules a k SP.Fact{ SP.verb=SP.CanSay{ SP.what=f' }}
  = step2b' 1 rules a k f'

step2b' i rules a k f@SP.Fact{ SP.verb=SP.CanSay{ SP.what=f' }} = do
  r <- step2bRule i a k f
  step2b' (i+1) (r:rules) a k f'

step2b' i rules a k f = do
  r <- step2bRule i a k f 
  return $ r:rules

step2bRule :: Int -> E -> [SP.D] -> SP.Fact -> FreshState Rule
step2bRule i a k f = do
  let h  = Says a kInf f
  let k' = k !! (i-1)
  x <- getFresh
  let b1 = Says x (toEnt k') f
  let b2 = Says a kInf SP.Fact{ SP.subject=x
                              , SP.verb=SP.CanSay{ SP.delegation=k'
                                                 , SP.what=f
                                                 }
                              }
  let r = Rule h [b1,b2] (SP.Boolean True)
  return r

ks :: SP.Assertion -> [SP.D]
ks SP.Assertion{ SP.says=SP.Claim{ SP.fact=f } }
  = ks' [] f

k0, kInf :: E
k0   = Constant "secpal:zero"
kInf = Constant "secpal:infinity"

toEnt :: SP.D -> E
toEnt (SP.Zero)     = k0
toEnt (SP.Infinity) = kInf

ks' :: [SP.D] -> SP.Fact -> [SP.D]
ks' xs SP.Fact{ SP.verb=SP.CanSay{ SP.delegation=d, SP.what=w }} =
  ks' (d:xs) w
ks' xs SP.Fact{} = xs


{- 3. Finally, for each Datalog rule created above of the form 
 -   A saysk e verbphrase ← ... 
 - we add a rule 
 -   A saysk x verbphrase ← 
 -     A saysk x can act as e, 
 -     A saysk e verbphrase
 - where x is a fresh variable. Note that k is not a fresh variable, but either
 - a constant or a variable taken from the original rule.  
 -}
step3 :: [Rule] -> FreshState [Rule]
step3 = (concat <$>) . mapM step3'

step3' :: Rule -> FreshState [Rule]
step3' rule@(Rule (Says a k f@SP.Fact{ SP.subject=e, SP.verb=vp }) _ _) = do
  x <- getFresh
  let h = Says a k SP.Fact{ SP.subject=x, SP.verb=vp }
  let b1 = Says a k SP.Fact{ SP.subject=x, SP.verb=SP.CanActAs{ SP.whom=e }}
  let b2 = Says a k f

  return [rule, Rule h [b1,b2] (SP.Boolean True)]
  


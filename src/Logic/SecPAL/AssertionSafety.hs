module Logic.SecPAL.AssertionSafety where

import Logic.General.Entities
import Logic.SecPAL.Language
import Logic.General.Vars

-- We say that a fact is flat when it does not contain can say, and is
-- nested otherwise. Facts are of the general form e1 can sayD1 ... en can
-- sayDn fact , where n ≥ 0 and fact is flat. For example, the fact Bob can
-- read f is flat, but Charlie can say0 Bob can read
-- f is not.

flat :: Fact -> Bool
flat Fact {verb=v} = case v of
                       CanSay {} -> False
                       _ -> True


-- Definition 2.6. (Assertion safety) 
-- Let A says fact if fact1,...,factn,c be an assertion. 
-- A variable x ∈ vars(fact) is safe iff x ∈ vars(fact1) ∪ ... ∪ vars(factn).
-- The assertion A says fact if fact1, ..., factn, c is safe iff
--   1. if fact is flat, all variables in vars(fact) are safe;
--      otherwise (i.e. fact is of the form e can sayD fact′) e is either a constant or a safe variable;
--   2. vars(c) ⊆ vars(fact) ∪ vars(fact1) ∪ ... ∪ vars(factn);
--   3. fact1,...,factn are flat.
safeIn :: E -> Claim -> Bool
safeIn Constant {} _ = True
safeIn v@(Variable {}) Claim { fact=f, conditions=fs }
  | v `elem` vars f = v `elem` concatMap vars fs
  | otherwise = True 

safe :: Assertion -> Bool
safe Assertion { says = claim@Claim{ fact=f@(Fact{subject=subj})
                                   , conditions=fs
                                   , constraint=c }
               } = 
    condition1 && condition2 && condition3
  where
    condition1 = if flat f 
                   then all (`safeIn` claim) (vars f)
                   else subj `safeIn` claim
    condition2 = all (`elem` concatMap vars fs) (vars c)
    condition3 = all flat fs


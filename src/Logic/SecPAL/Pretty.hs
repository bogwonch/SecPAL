module Logic.SecPAL.Pretty where

import Logic.General.Constraints
import Logic.General.Pretty
import Logic.SecPAL.Language
import Data.List

instance PShow D where
  pShow Zero = "0"
  pShow Infinity = "inf"

instance PShow VerbPhrase where
  pShow Predicate{predicate=p, args=as} = p ++ "(" ++ intercalate ", " (map pShow as) ++ ")"
  pShow CanSay{delegation=de, what=w} = "can-say "++pShow de++" "++pShow w
  pShow CanActAs{whom=w} = "can-act-as "++pShow w
  
instance PShow Fact where
  pShow Fact{subject=s, verb=vp} = pShow s++" "++pShow vp

instance PShow Claim where    
  pShow Claim{fact=f, conditions=fs, constraint=c'} = 
    case (fs, c') of
      ([], Boolean True) -> pShow f
      (_,  Boolean True) -> pShow f++" if "++intercalate ", " (map pShow fs)
      ([], _)            -> pShow f++"; "++pShow c'
      _                  -> pShow f++" if "++intercalate ", " (map pShow fs)++"; "++pShow c'

instance PShow Assertion where
  pShow Assertion{who=w, says=s} = pShow w++" says "++pShow s++"."

instance PShow AC where
  pShow (AC a) = "{ " ++ intercalate ", " (map pShow a) ++ " }"



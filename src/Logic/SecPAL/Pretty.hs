module Logic.SecPAL.Pretty where

import Logic.SecPAL.Language
import Data.List

class PShow a where
  pShow :: a -> String

instance PShow a => PShow (Maybe a) where 
  pShow (Just a) = pShow a
  pShow Nothing = ":-("

instance PShow a => PShow [a] where
  pShow xs = "[" ++ intercalate ", " (map pShow xs) ++ "]"


instance PShow E where
  pShow (Variable n) = n
  pShow (Constant n) = n

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

instance PShow F where
  pShow (F name) = name


instance PShow Ec where
  pShow (Entity e) = pShow e
  pShow (Apply f xs) = pShow f++"("++intercalate ", " (map pShow xs)++")"
  pShow (Value v) = pShow v

instance PShow C where
  --pShow (Boolean True) = "⊤"
  pShow (Boolean True) = "True"
  --pShow (Boolean False) = "⊥"
  pShow (Boolean False) = "False"
  pShow (Equals x y) = pShow x++" = "++pShow y
  --pShow (Not (Equals x y)) = pShow x++"≠"++pShow y
  --pShow (Not x) = "¬ "++pShow x
  pShow (Not x) = "! "++pShow x
  --pShow (Conj x y) = pShow x++" ⋀ "++pShow y
  pShow (Conj x y) = pShow x++", "++pShow y

instance PShow Value where
    pShow (Int' x) = show x
    pShow (Float' x) = show x
    pShow (String' x) = show x



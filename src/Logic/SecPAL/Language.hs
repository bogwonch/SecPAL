module Logic.SecPAL.Language where

import Data.Char
import Data.List

class PShow a where
  pShow :: a -> String

data E = Variable { varName :: String }
       | Constant { constName :: String }
       deriving (Eq,Show)

instance PShow E where
  pShow (Variable n) = n
  pShow (Constant n) = n

data D = Zero
       | Infinity
       deriving (Eq,Show)

instance PShow D where
  pShow Zero = "0"
  pShow Infinity = "inf"

data VerbPhrase = Predicate { predicate :: String, args :: [E] }
                | CanSay { delegation :: D, what :: Fact }
                | CanActAs { whom :: E }
                deriving (Eq,Show)

instance PShow VerbPhrase where
  pShow Predicate{predicate=p, args=as} = p ++ "(" ++ intercalate ", " (map pShow as) ++ ")"
  pShow CanSay{delegation=d, what=w} = "can-say "++pShow d++" "++pShow w
  pShow CanActAs{whom=w} = "can-act-as "++pShow w
  
data Fact = Fact { subject :: E, verb :: VerbPhrase }
          deriving (Eq,Show)

instance PShow Fact where
  pShow Fact{subject=s, verb=vp} = pShow s++" "++pShow vp

data Claim = Claim { fact :: Fact, conditions :: [Fact], constraint :: C }
           deriving (Eq,Show)

instance PShow Claim where    
  pShow Claim{fact=f, conditions=fs, constraint=c} = 
    case (fs, c) of
      ([], Boolean True) -> pShow f
      (_,  Boolean True) -> pShow f++" if "++intercalate ", " (map pShow fs)
      ([], c)            -> pShow f++"; "++pShow c
      otherwise          -> pShow f++" if "++intercalate ", " (map pShow fs)++"; "++pShow c

data Assertion = Assertion { who :: E, says :: Claim }
               deriving (Eq,Show)

instance PShow Assertion where
  pShow Assertion{who=w, says=s} = pShow w++" says "++pShow s++"."

data AC = AC [Assertion]
        deriving (Eq,Show)

instance PShow AC where
  pShow (AC acs) = "{ " ++ intercalate ", " (map pShow acs) ++ " }"

acs :: AC -> [Assertion]
acs (AC as) = as

data F = F { fName :: String }
       deriving (Eq,Show)

instance PShow F where
  pShow (F name) = name


data Ec = Entity E
        | Apply F [Ec]
        | Value Value
        deriving (Eq,Show)

instance PShow Ec where
  pShow (Entity e) = pShow e
  pShow (Apply f xs) = pShow f++"("++intercalate ", " (map pShow xs)++")"
  pShow (Value v) = pShow v

data C = Boolean Bool
       | Equals Ec Ec
       | Not C
       | Conj C C
       deriving (Eq,Show)

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

data Value 
    = Int' Int
    | Float' Float
    | String' String
  deriving (Eq,Show)

instance PShow Value where
    pShow (Int' x) = show x
    pShow (Float' x) = show x
    pShow (String' x) = show x


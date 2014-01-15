module Logic.SecPAL.Language where

import Data.Char
import Data.List

data E = Variable { varName :: String }
       | Constant { constName :: String }
       deriving (Eq)

instance Show E where
  show (Variable n) = map toLower n
  show (Constant (n:ns)) = toTitle n : ns

data D = Zero
       | Infinity
       deriving (Eq)

instance Show D where
  show Zero = "𝟶"
  show Infinity = "∞"

data VerbPhrase = Predicate { predicate :: String, args :: [E] }
                | CanSay { delegation :: D, what :: Fact }
                | CanActAs { whom :: E }
                deriving (Eq)

instance Show VerbPhrase where
  show Predicate{predicate=p, args=as} = p ++ "(" ++ intercalate ", " (map show as) ++ ")"
  show CanSay{delegation=d, what=w} = "can say"++show d++" "++show w
  
data Fact = Fact { subject :: E, verb :: VerbPhrase }
          deriving (Eq)

instance Show Fact where
  show Fact{subject=s, verb=vp} = show s++" "++show vp

data Claim = Claim { fact :: Fact, conditions :: [Fact], constraint :: C }
           deriving (Eq)

instance Show Claim where
  show Claim{fact=f, conditions=[], constraint=Boolean True} = 
    show f
  show Claim{fact=f, conditions=fs, constraint=Boolean True} = 
    show f++" if "++intercalate ", " (map show fs)
  show Claim{fact=f, conditions=fs, constraint=c} = 
    show f++" if "++intercalate ", " (map show fs)++", "++show c

data Assertion = Assertion { who :: E, says :: Claim }
               deriving (Eq)

instance Show Assertion where
  show Assertion{who=w, says=s} = show w++" says "++show s++"."

data AC = AC [Assertion]
        deriving (Eq, Show)

data F = F { fName :: String }
       deriving (Eq)

instance Show F where
  show (F name) = name


data Ec = Entity E
        | Apply F [E]
        deriving (Eq)

instance Show Ec where
  show (Entity e) = show e
  show (Apply f xs) = show f++"("++intercalate ", " (map show xs)++")"

data C = Boolean Bool
       | Equals Ec Ec
       | Not C
       | Conj C C
       deriving (Eq)

instance Show C where
  show (Boolean True) = "⊤"
  show (Boolean False) = "⊥"
  show (Equals x y) = show x++"="++show y
  show (Not (Equals x y)) = show x++"≠"++show y
  show (Not x) = "¬ "++show x
  show (Conj x y) = show x++" ⋀ "++show y



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
  show Zero = "0"
  show Infinity = "∞"

data VerbPhrase = Predicate { predicate :: String, args :: [E] }
                | CanSay { delegation :: D, what :: Fact }
                | CanActAs { whom :: E }
                deriving (Eq)

instance Show VerbPhrase where
  show Predicate{predicate=p, args=as} = p ++ "(" ++ intercalate ", " (map show as) ++ ")"
  show CanSay{delegation=d, what=w} = "can say"++show d++" "++show w
  show CanActAs{whom=w} = "can act as "++show w
  
data Fact = Fact { subject :: E, verb :: VerbPhrase }
          deriving (Eq)

instance Show Fact where
  show Fact{subject=s, verb=vp} = show s++" "++show vp

data Claim = Claim { fact :: Fact, conditions :: [Fact], constraint :: C }
           deriving (Eq)

instance Show Claim where    
  show Claim{fact=f, conditions=fs, constraint=c} = 
    case (fs, c) of
      ([], Boolean True) -> show f
      (_,  Boolean True) -> show f++" if "++intercalate ", " (map show fs)
      otherwise          -> show f++" if "++intercalate ", " (map show fs)++", "++show c

data Assertion = Assertion { who :: E, says :: Claim }
               deriving (Eq)

instance Show Assertion where
  show Assertion{who=w, says=s} = show w++" says "++show s++"."

data AC = AC [Assertion]
        deriving (Eq)

instance Show AC where
  show (AC acs) = "{ " ++ intercalate ", " (map show acs) ++ " }"

acs :: AC -> [Assertion]
acs (AC as) = as

data F = F { fName :: String }
       deriving (Eq)

instance Show F where
  show (F name) = name


data Ec = Entity E
        | Apply F [Ec]
        | Value Value
        deriving (Eq)

instance Show Ec where
  show (Entity e) = show e
  show (Apply f xs) = show f++"("++intercalate ", " (map show xs)++")"
  show (Value v) = show v

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

data Value 
    = Int' Int
    | Float' Float
    | String' String
  deriving (Eq)

instance Show Value where
    show (Int' x) = show x
    show (Float' x) = show x
    show (String' x) = show x



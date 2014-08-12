module Logic.General.Pretty where

import Logic.General.Entities
import Logic.General.Constraints
import Data.List

class PShow a where
  pShow :: a -> String

instance PShow a => PShow (Maybe a) where 
  pShow (Just a) = pShow a
  pShow Nothing = ":-("

instance PShow a => PShow [a] where
  pShow xs = "[" ++ intercalate ", " (map pShow xs) ++ "]"


instance PShow E where
  pShow (Variable n _) = n
  pShow (Constant n _) = n

instance PShow F where
  pShow (F name) = name

instance PShow Ec where
  pShow (Entity e) = pShow e
  pShow (Apply f xs) = pShow f++"("++intercalate ", " (map pShow xs)++")"
  pShow (Value v) = pShow v

instance PShow C where
  pShow (Boolean True) = "True"
  pShow (Boolean False) = "False"
  pShow (Equals x y) = pShow x++" = "++pShow y
  pShow (Not x) = "! "++pShow x
  pShow (Conj x y) = pShow x++", "++pShow y
  pShow (LessThan x y) = pShow x++" < "++pShow y
  pShow (GreaterThan x y) = pShow x++" > "++pShow y

instance PShow Value where
    pShow (Int' x) = show x
    pShow (Float' x) = show x
    pShow (String' x) = show x
    pShow (Bool' x) = show x



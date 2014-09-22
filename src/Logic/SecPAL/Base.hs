{-- Gives a default values for constraint types.
 - May be redundant now
 -}
module Logic.SecPAL.Base where

import Logic.SecPAL.Language

class Base a where
  base :: a -> a

instance Base Value where
  base (Int' _)    = Int' 0
  base (Float' _)  = Float' 0
  base (String' _) = String' ""
  base (Bool' _)   = Bool' True


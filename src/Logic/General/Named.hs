{-- Get the names of entities and functions
 - TODO: make sure we use this api rather than getting directly
 -}
module Logic.General.Named where
  
import Logic.General.Constraints
import Logic.General.Entities

class Named a where
    name :: a -> String

instance Named E where 
    name Variable {varName=n} = n
    name Constant {constName=n} = n

instance Named F where
    name F {fName=n} = n


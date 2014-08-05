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


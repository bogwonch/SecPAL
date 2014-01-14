module Logic.SecPAL.Named where
  
import Logic.SecPAL.Language

class Named a where
    name :: a -> String

instance Named E where 
    name Variable {varName=n} = n
    name Constant {constName=n} = n

instance Named F where
    name F {fName=n} = n


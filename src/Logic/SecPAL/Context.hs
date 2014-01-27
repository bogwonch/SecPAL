module Logic.SecPAL.Context where

import Logic.SecPAL.Language

data Context = Context { ac :: AC
                       , d :: D
                       }
  deriving (Eq,Show)


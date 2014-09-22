{-- Generate fresh variables
 - POSSIBLY REDUNDANT (used for DatalogC)
 -}
module Logic.General.Fresh where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State
import Logic.General.Entities

type Fresh = (String, Integer)
type FreshState = State Fresh

freshVar :: Fresh -> E
freshVar (prefix, n) = Variable{ varType=prefix
                               , varName=prefix ++ ":" ++ show n
                               }

next :: FreshState ()
next = modify $ second (1+)

getFresh :: FreshState E
getFresh = freshVar <$> get <* next

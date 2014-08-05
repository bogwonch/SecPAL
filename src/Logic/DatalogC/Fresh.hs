module Logic.DatalogC.Fresh	where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State
import Logic.General.Entities

type Fresh = (String, Integer)
type FreshState = State Fresh

freshVar :: Fresh -> E
freshVar (prefix, n) = Variable $ prefix ++ show n

next :: FreshState ()
next = modify $ second (1+)

getFresh :: FreshState E
getFresh = freshVar <$> get <* next
 

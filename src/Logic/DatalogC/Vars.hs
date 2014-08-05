module Logic.DatalogC.Vars where

import Logic.General.Entities
import qualified Logic.DatalogC.Language as L

class Vars a where
  vars :: a -> [E]

instance Vars E where
  vars e@Variable{}  = [e]
  vars Constant{} = []

instance Vars L.Constraint where
  vars _ = [] -- TODO: Constraints

instance Vars L.Predicate where
  vars L.Predicate{ L.args=as } = concatMap vars as

instance Vars L.Clause where 
  vars c = vars (L.head c) ++ vars (L.body c) ++ vars (L.constraint c)

instance Vars x => Vars [x] where
  vars = concatMap vars

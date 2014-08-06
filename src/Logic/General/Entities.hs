module Logic.General.Entities where

import Logic.General.Types

data E = Variable { varName :: String, varType :: String }
       | Constant { constName :: String, constType :: String }
       deriving (Show)

instance Eq E where
    (Variable _ _) == (Constant _ _) = False
    (Constant _ _) == (Variable _ _) = False
    (Variable x _) == (Variable y _) = x == y
    (Constant x _) == (Constant y _) = x == y

instance Typed E where
  typeof Variable{ varType   = t } = t
  typeof Constant{ constType = t } = t


module Logic.General.Entities where

data E = Variable { varName :: String }
       | Constant { constName :: String }
       deriving (Show)

instance Eq E where
    (Variable _) == (Constant _) = False
    (Constant _) == (Variable _) = False
    (Variable x) == (Variable y) = x == y
    (Constant x) == (Constant y) = x == y

